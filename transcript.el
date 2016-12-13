;;; transcript.el --- edit transcripts of sound recordings

;; Author: Kilian A. Foth <foth@informatik.uni-hamburg.de>
;; Created: 10. Jul 2001
;; Version: 0.4
;; Keywords: editing, playback

;; This file is not part of anything larger than itself.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Change Log:

;; 0.1 Initial version
;; 0.2 add .AVI support
;; 0.3 add tm-mplayer-protocol
;; 0.4 adapt to mplayer's new and improved input methods
;; 0.5 remove the need for yahwe as I can't find it anywhere.

;;; Commentary:

;; This file defines transcript mode, a set of utilities
;; to edit movie transcripts more easily.

;; Most importantly, the function tm-play starts a playback program
;; that can be controlled from within the emacs buffer. The player
;; program can then be controlled from within emacs with the movement
;; keys. At the moment, protocols are defined for talking to .AVI,
;; .WAV, and .MP3 players, but pretty much any media format can be
;; plugged in if only it has a replayer that supports remote-control
;; mode (i.e., takes commands from stdin).

;; Some functions are also provided for repetitive tasks when typing
;; transcripts, such as inserting speech prefixes semi-automatically.

;;; Code:
(require 'cl)
(require 'derived)


;; (defvar tm-keys
;;   '(([insert]           . tm-play)
;;     ([delete]           . tm-rewind)
;;     ([home]             . tm-home)
;;     ([end]              . tm-end)
;;     ([prior]            . tm-prior)
;;     ([next]             . tm-next)
;;     ([escape]           . tm-quit)
;;     ([up]               . tm-up)
;;     ([down]             . tm-down)
;;     ([left]             . tm-left)
;;     ([right]            . tm-right)
;;     ([(control insert)] . tm-insert-timestamp)
;;     ([kp-left]          . tm-left-channel)
;;     ([kp-begin]         . tm-both-channels)
;;     ([kp-right]         . tm-right-channel)
;;     ([kp-add]           . tm-raise-volume)
;;     ([kp-subtract]      . tm-lower-volume)
;;     ([(control return)] . tm-insert-attribution)
;;     ([?8]               . tm-stress-last-word)
;;     ([(control ?.)]     . tm-insert-ellipsis)))


(defun make-time-float (list)
  (+ (* 65536.0 (pop list)) (pop list)
     (/ (or (pop list) 0) 1e6)))

(defvar last-mode-line-update (make-time-float (current-time)))

(defvar tm-keys
  '(
    ([insert]           . tm-play)
    ([delete]           . tm-rewind)
    ([(control insert)] . tm-insert-timestamp)

	))

(define-derived-mode transcript-mode text-mode "Transcript"
  "Major mode for editing dialogue transcripts."

  ;; Load a dictionary of common words
  ;; (extremely useful in connection with dabbrev)
  (when (file-readable-p tm-dictionary)
    (find-file-noselect tm-dictionary))

  ;; intercept motion keys during playback
  (dolist (key tm-keys)
    (define-key transcript-mode-map (car key) (cdr key)))
  
  (setq tm-initialized-p nil))
 

(defgroup transcript nil
  "Customization of transcript-mode variables."
  :group 'tools)


;;;; Definitions for integrated sound playback

;; These commands are for Linux.
;; Substitute your favorite batch mixer otherwise.
(defcustom tm-mixer-volume-command
  "/usr/X11R6/bin/smix -q -s 0 =%d"
  "*Command to set mixer level."
  :type 'string
  :group 'transcript)

(defcustom tm-mixer-balance-command
  "/usr/X11R6/bin/smix -q -s 1 =%d =%d"
  "*Command to set mixer balance."
  :type 'string
  :group 'transcript)

(defcustom tm-mixer-level 40
  "Default mixer level."
  :type 'number
  :group 'transcript)

(defun tm-set-volume ()
  "Set replay volume to `tm-mixer-level'."
  (shell-command
   (format tm-mixer-volume-command tm-mixer-level))
  (message "volume %d" tm-mixer-level))

(defun tm-set-balance (x)
  "Select replay of left, right, or both channels."
  (shell-command
   (case x
     (-1 (format tm-mixer-balance-command tm-mixer-level  1))
     ( 0 (format tm-mixer-balance-command tm-mixer-level tm-mixer-level))
     ( 1 (format tm-mixer-balance-command  1 tm-mixer-level))))
  (case x
    (-1 (message "Replaying left channel."))
    ( 0 (message "Replaying both channels."))
    ( 1 (message "Replaying right channel."))))

;;; Protocols

;; mpg123

;; mpg123 comes with a remote mode that fits right into transcript-mode.
(defvar tm-mpg123-protocol
  '((program . "/usr/local/bin/mpg123")
    (options . "-R")
    (filter . tm-mpg123-filter)
    (load . "load %s\n")
    (play . "pause\n")
    (quit . "quit\n")
    (seek . tm-mpg123-seek)
    (skip . tm-mpg123-skip)
    (stop . "pause\n"))
  "The protocol spoken by mpg123.")


(defun tm-mpg123-filter (process line)
  "Interpret mpg123 output."
  
  ;; Status message during playing (frame info). Example:
  ;; @F 5 65480 0.13 1710.50
  (if (string= (substring line 0 2) "@F")
      (progn
        (string-match
         "\\([0-9]+\\) \\([0-9]+\\) \\([0-9.]+\\) \\([0-9.]+\\)"
         line)
        (let ((current-frame (string-to-int (match-string 1 line)))
              (remaining-frames (string-to-int (match-string 2 line)))
              (current-sec (string-to-number (match-string 3 line)))
              (remaining-secs (string-to-number (match-string 4 line))))
          (setq tm-file-length (+ current-sec remaining-secs)
                tm-current-second current-sec
                tm-position (tm-format-time (round current-sec))
                tm-framerate (/ current-frame current-sec))
          (tm-draw-modeline)
          ))
;    (message "Got line `%s'" line)
    ))

;; mpg123 insists on measuring everything in frames,
;; so we have to multiply x before sending it.
(defun tm-mpg123-skip (x)
  "Tell mpg123 to skip X seconds."
  (format "jump %+d\n" (* tm-framerate x)))

(defun tm-mpg123-seek (x)
  "Tell mpg123 to seek to second X."
  (format "jump %d\n" (* tm-framerate x)))

;; aviplay

;; aviplay has a remote mode as well.
(defvar tm-aviplay-protocol
  '((program . "aviplay")
    (options . "-rc")
    (auto-rewind . -2)
    (filter . tm-aviplay-filter)
    (play . "p\n")
    (quit . "q\n")
    (seek . "r%d")
    (skip . tm-aviplay-skip)
    (stop . "p\n"))
  "The protocol spoken by aviplay.")

(defun tm-aviplay-filter (process line)
  "Interpret aviplay output."
;  (message "Got line `%s'" line)

  ;; Status message during playing. Example:
  ;; Pos: 125 s / 1710 s
  (if (string-match "^Pos: \\([0-9]+\\) s / \\([0-9]+\\) s" line)
      (setq tm-current-second (string-to-int (match-string 1 line))
            tm-file-length (string-to-int (match-string 1 line))
            tm-position (tm-format-time (round tm-current-second))))
  (tm-draw-modeline))

;; aviplay has no skip command, so we calculate
;; a fitting position for a seek command.
(defun tm-aviplay-skip (x)
  "Tell aviplay to skip X seconds."
  (format "r%d\n" (+ x tm-current-second)))

;; mplayer

;; MPlayer is now the video replayer of choice.
(defvar tm-mplayer-protocol
  '((program . "mplayer")
    (options . "-slave")
;    (auto-rewind . -1)
    (filter . tm-mplayer-filter)
    (play . "pause\n")
;; Grrrr. What I call `skip' (relative positioning), 
;; A'rpi calls `seek'.
    (skip . "seek %d\n") 
    (seek . tm-mplayer-seek) 
    (quit . "quit\n")
    (stop . "pause\n"))
  "The protocol spoken by mplayer.")

(defun tm-mplayer-filter (process line)
  "Interpret mplayer output."

  ;; Status message during playing.
  ;; Example: A:  21.6 V:  21.6 A-V:  0.001 ct:  0.008  541/541  11%  7%  1.6% 0 0
  (if (string-match "^A: *\\( [0-9.:]+\\)" line)
      (setq 
       tm-position  (match-string 1 line)
       )
    )
  (tm-draw-modeline)

;  (message "Got line `%s'" line)

)

(defun tm-mplayer-filter (process line)
   "Interpret mplayer output."

   ;; Status message during playing.
   ;; Example: A:  21.6 V:  21.6 A-V:  0.001 ct:  0.008  541/541  11%  7% 1.6% 0 0
   (if (string-match "^A: *\\([0-9.]+\\)" line)
       (setq tm-current-second (string-to-int (match-string 1 line))
             tm-position (tm-format-time (round tm-current-second))))
   (tm-draw-modeline)

 ;  (message "Got line `%s'" line)

 )

;; mplayer has no command for absolute positioning, so we calculate
;; a fitting offset for a relative command.
(defun tm-mplayer-seek (x)
  "Tell mplayer to jump to second X."
  (format "seek %d\n" (- x tm-current-second)))



(defcustom tm-players-alist
  '(
;    ("\\.avi$" . tm-aviplay-protocol)
    ("\\.avi$" . tm-mplayer-protocol)
;    ("\\.mp3$" . tm-mpg123-protocol)
    ("\\.mp3$" . tm-mplayer-protocol)
    ("\\.wav$" . tm-mplayer-protocol))
  "*Alist of replayable media formats.

Each entry should map a regular expression for filenames to a protocol
for handling that file type. Each protocol is the name of an alist
with the following keys:

program       Name of executable to start
options       List of command line options to the program
filter        Filter function for parsing the program's output
auto-rewind   If the replayer is so unresponsive that many frames are
              lost between a stop command and the actual stopping,
              set this to the number of seconds to repeat after a stop.
load          Command for loading the sound file
max           Command for querying the length of the sound file
play          Command for resuming playback
pos           Command for querying the current position
quit          Command for quitting the program
seek          Command for absolute seek
skip          Command for relative seek
stop          Command for pausing the player
  
Each command will be sent to the player's STDIN. SEEK and SKIP will be
run through the format function with an argument indicating number of
seconds. Instead of a string, a symbol may be given. It should be the
name of a function that will be called with the same arguments to
generate the string.

If a key is missing from the protocol, no command is sent. Example:
mpg123 doesn't need the pos command because it continually prints the
current position as it is.")


;; variables local to each transcript buffer
(defvar tm-sound-file nil
  "*Sound file to transcribe.")
(make-variable-buffer-local 'tm-sound-file)

(defvar tm-framerate 44100
  "Frame rate of the sound file.")
(make-variable-buffer-local 'tm-framerate)

(defvar tm-initialized-p nil
  "Has playback been set up in this buffer?")
(make-variable-buffer-local 'tm-initialized-p)

(defvar tm-protocol nil
  "Alist of commands for talking to the playback program.")
(make-variable-buffer-local 'tm-protocol)

(defvar tm-player-name nil
  "Name of player program for transcript mode.")
(make-variable-buffer-local 'tm-player-name)

(defvar tm-player-options nil
  "*String of options to pass to the player program in transcript mode.")
(make-variable-buffer-local 'tm-player-options)

(defvar tm-filter nil
  "Name of filter function for transcript mode.")
(make-variable-buffer-local 'tm-filter)

(defvar tm-status 'null
  "Status of sound playback:

null - no player process
idle - player is paused
play - player is playing sound")
(make-variable-buffer-local 'tm-status)

(defvar tm-process nil
  "XEmacs process for the sound player.")
(make-variable-buffer-local 'tm-process)

(defvar tm-position ""
  "Current position in sound file as a string.")
(make-variable-buffer-local 'tm-position)

(defvar tm-current-second 0
  "Current position in sound file as an integer.")
(make-variable-buffer-local 'tm-current-second)

(defvar tm-file-length 0
  "Length of the sound file in seconds.")
(make-variable-buffer-local 'tm-file-length)

(defvar tm-playback-position 0
  "Last position in playback.")
(make-variable-buffer-local 'tm-playback-position)

(defvar tm-point 0
  "Last position in buffer.")
(make-variable-buffer-local 'tm-point)

(defun tm-setup-playback ()
  "Choose player and protocol based on the name of the sound file."
  (if (null tm-sound-file)
      (error "No sound file given, can't setup playback."))
  (setq tm-protocol nil)
  (let ((l tm-players-alist))
    (while (and l (null tm-protocol))
      (if (string-match (caar l) tm-sound-file)
          (setq tm-protocol (eval (cdar l))))
      (setq l (cdr l)))
    )
  (if (null tm-protocol)
      (error (concat "Don't know how to play back file `" tm-sound-file "'!")))
  (goto-char tm-point)
  (setq tm-player-name (cdr (assoc 'program tm-protocol))
        tm-player-options (cdr (assoc 'options tm-protocol))
        tm-filter (cdr (assoc 'filter tm-protocol))
        tm-auto-rewind (cdr (assoc 'auto-rewind tm-protocol))
        tm-initialized-p t))

(defun tm-maybe-start-process ()
  "Try to start an external sound player."
  (unless tm-initialized-p
    (tm-setup-playback))
  
  (unless (file-readable-p tm-sound-file)
    (error (concat "Can't read sound file " tm-sound-file)))

  ;; kill process if necessary
  (if (and tm-process
           (not (eq (process-status tm-process) 'run)))
      (progn
        (delete-process "playback")
        (setq tm-process nil)))

  ;; start afresh
  (if (null tm-process)
      (progn
        (setq tm-process
              (start-process "playback" nil
                             tm-player-name
                             tm-player-options
                             tm-sound-file)
              tm-status 'play)
        (set-process-filter tm-process tm-filter)
        (set-process-sentinel tm-process 'tm-process-sentinel)
        ;; some programs take the file name on the command line,
        ;; others (mpg123) need an explicit load command.
        (tm-issue 'load tm-sound-file)
        (tm-issue 'seek tm-playback-position))))

(defun tm-process-sentinel (p change)
  "Function to report status changes of the playback process."
  ;; mplayer always exits with 1, should be 0
  (if (and (string= tm-player-name "mplayer")
           (string-match "exited abnormally with code 1"  change))
      (setq change "finished."))
  ;; notify user
  (message (concat "Player process: " change))
  ;; notify self
  (unless (and tm-process
               (eq 'run (process-status tm-process)))
    (setq tm-process nil
          tm-status 'null)))

(defun tm-issue (x &rest args)
  "Send a command to the playback process.

COMMAND is a symbol that indicates the type of command.
ARGS contains additional data.

A typical invocation would be (tm-issue 'seek 120).

The exact command to send is decoded from the current `tm-protocol'."
  (let ((command (cdr (assoc x tm-protocol))))
    (when command
      (progn
        (if (functionp command)
            (setq command (apply command args)))
        (if (stringp command)
            (progn
              (setq command (apply 'format (cons command args)))
              (process-send-string tm-process command)
              (message "Sent: %s" command)
              )
          (message "WARNING: couldn't issue command %s!" (symbol-name x))
          )))))
  
 
;; Functions for controlling playback
(defun tm-raise-volume ()
  "Raise volume of sound playback."
  (interactive)
  (if (< tm-mixer-level 100)
      (incf tm-mixer-level 5))
  (tm-set-volume))

(defun tm-lower-volume ()
  "Lower volume of sound playback."
  (interactive)
  (if (> tm-mixer-level 1)
      (decf tm-mixer-level 5))
  (tm-set-volume))

(defun tm-left-channel ()
  "Play only left channel."
  (interactive)
  (tm-set-balance -1))

(defun tm-both-channels ()
  "Play both channels."
  (interactive)
  (tm-set-balance 0))

(defun tm-right-channel ()
  "Play only right channel."
  (interactive)
  (tm-set-balance 1))

(defun tm-play ()
  "Play back the sound file at current position."
  (interactive)
  (case tm-status
    ('null
     (tm-maybe-start-process)
     (setq tm-status 'play)
     (message "Starting playback..."))
    ('idle
     (tm-issue 'play tm-sound-file)
     (setq tm-status 'play)
     (message "Resuming..."))
    ('play
     ;; rewind a bit to protect against lost sound during
     ;; the switchoff. Professional dictation systems do it that way :-)
     (if tm-auto-rewind
         (tm-issue 'skip tm-auto-rewind))
     (tm-issue 'stop)
     (setq tm-status 'idle)
     (message "Stopped at %s." (tm-where)))))
    
(defun tm-rewind () "Stop playback and rewind a bit." (interactive)
  (if (eq tm-status 'play)
      (tm-skip -3)
    (backward-or-forward-delete-char 1)))

(defun tm-home () "Rewind player." (interactive)
  (if (eq tm-status 'play)
      (tm-seek 0)
    (funcall (lookup-key (current-global-map) [home]))))
    
(defun tm-end () "Jump to end of sound file." (interactive)
  (if (eq tm-status 'play)
      (tm-seek (- (tm-query-length) 3)))
  (funcall (lookup-key (current-global-map) [end])))

(defun tm-prior () "Rewind an hour." (interactive)
  (if (eq tm-status 'play)
      (tm-skip -3600)
    (scroll-down-command)))

(defun tm-next () "Forward an hour." (interactive)
  (if (eq tm-status 'play)
      (tm-skip 3600)
    (scroll-up-command)))

(defun tm-up () "Rewind a minute." (interactive)
  (if (eq tm-status 'play)
      (tm-skip -60)
    (funcall (lookup-key (current-global-map) [up]) 1)))

(defun tm-down () "Forward a minute." (interactive)
  (if (eq tm-status 'play)
      (tm-skip 60)
    (funcall (lookup-key (current-global-map) [down]) 1)))

(defun tm-right () "Forward a bit." (interactive)
  (if (eq tm-status 'play)
      (tm-skip 5)
    (funcall (lookup-key (current-global-map) [right]) 1)))

(defun tm-left () "Rewind a minute." (interactive)
  (if (eq tm-status 'play)
      (tm-skip -5)
    (funcall (lookup-key (current-global-map) [left]) 1)))

(defun tm-insert-timestamp ()
  "Insert the current position in the sound file."
  (interactive)
;  (if (eq tm-status 'play)
      (insert tm-position)
;    (error "No playback running."))
)

(defun tm-query-length ()
  "Query file position from the player."
  (interactive)
  (if (null tm-process)
      (error "No playback process."))
  (tm-issue 'max)
  (accept-process-output tm-process 1)
  tm-file-length)
 
(defun tm-quit ()
  "Kill the playback process."
  (interactive)
  (unless (eq tm-status 'null)
    (progn
      (tm-write-position)
      (tm-issue 'quit)
      (setq tm-status 'null
            tm-process nil
            modeline-process ""))))

(defun tm-write-position ()
  "Record the current position on the local variables section."
  (setq tm-playback-position tm-current-second)
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "tm-playback-position: " nil t)
        (progn
          (kill-line)
          (insert (number-to-string tm-playback-position)))
      (error "Can't save playback position, your local variables don't mention one."))
    )
  (save-excursion
  (setq tm-point (point))
    (beginning-of-buffer)
    (if (re-search-forward "tm-point: " nil t)
        (progn
          (kill-line)
          (insert (number-to-string tm-point)))
      (error "Can't save file position, your local variables don't mention one."))
    ))

(defun tm-seek (where)
  "Move absolutely in the playback."
  (tm-issue 'seek where)

  )

(defun tm-skip (x)
  "Move relatively in the playback."
  (progn
    (tm-issue 'skip x)
    (message "Playing at %s." (tm-where))
    )
  )

(defun tm-where ()
  "Query file position from the player."
  (interactive)
  (if (null tm-process)
      (error "No playback process."))
  (tm-issue 'pos)
  (accept-process-output tm-process 1)
  tm-position)

(defun tm-format-time (seconds)
  "Transform 123 --> `[00:02:03]'."
  (format "%02d:%02d:%02d"
          (/ seconds 60 60)
          (% (/ seconds 60) 60)
          (% seconds 60)))

(defun tm-parse-time (line)
  "Transform `[00:02:03]' --> 123."
;  (let ((result 0))
;    (while (string-match "\\([0-9]+\\):?" line)
;      (setq result (+ (* 60 result)
;                      (string-to-int (match-string 1 line)))
;            line (substring line (length (match-string 0 line)))))
;    result)
  line
  )

(defun tm-draw-modeline ()
  "Display status of playback process in the modeline."
  (progn

   (if (featurep 'xemacs)
       (unless (eq 'null tm-status)
	 (progn
	   (setq modeline-process tm-position)
	   (redraw-mode-line))))

; I don't know how to do this correctly under gnu emacs.
; The current solution produces a lot of flicker when running under X but looks OK on the console.
; It taxes the CPU heavily in either mode.
; The 0.1 seconds wait reduces the load from 50% to about 10% on a Pentium III 500 MHz
; Oivvio Polite <m4@polite.se>

   (if (not (featurep 'xemacs))
       (unless (eq 'null tm-status)
	 (progn
	   (setq now (make-time-float (current-time)))
	   (if (> (- now last-mode-line-update) 0.1)
	       (progn
		(setq  mode-line-process tm-position)
		(force-mode-line-update)
		(setq last-mode-line-update (make-time-float (current-time)))		
		))


	   )))
   )
  )


;(defun tm-draw-modeline ()
;  "Display status of playback process in the modeline."
;  (if (featurep 'xemacs)
;      (unless (eq 'null tm-status)
;        (progn
;          (setq modeline-process tm-position)
;          (redraw-mode-line))))
;  )

 
;;;; Definitions for easier typing
(defcustom tm-dictionary "/usr/share/dict/words"
  "*Dictionary loaded when entering transcript mode."
  :type 'string
  :group 'transcript)

(defun tm-insert-ellipsis ()
  "Cleverly insert ... with or without space."
  (interactive)
  (let ((initial (= ?\ (char-syntax (char-before)))))
    (insert "..." (if initial "" " "))))

(defun wrap-n-last-words (open close n)
  "Put delimiters around N last words."
  (skip-chars-backward "\n ")
  (let ((expression
         (progn
           (let ((beg (point)))
             ;; skip n words backwards
             (search-backward-regexp "^\\| " nil nil n)
             (skip-chars-forward " ")
             ;; delete & remember N last words
             (kill-region beg (point))
             (current-kill 0)))))
    ;; re-insert delimited expression
    (insert open expression close)))

(defun tm-stress-last-word (&optional n)
  "Wrap ** around N last words."
  (interactive "*P")
  (wrap-n-last-words "*" "*" n)
  (unless (looking-at "[\"',.;?!:]")
    (just-one-space)))

(defvar tm-attributions '()
  "List of dialogue attributions already used.")
(make-variable-buffer-local 'tm-attributions)

(defvar tm-attribution-regexp "^[a-z0-9 ]+: "
  "Regexp for recognizing dialogue attributions.")

(defun tm-insert-attribution ()
  "Insert dialogue attribution, based on who had the last turn.

Successive invocations cycle through the available attributions."
  (interactive)
  (end-of-line)
  (let ((case-fold-search nil)
        (init (not (eq last-command 'tm-insert-attribution))))
    (if init
        (progn
          ;; the first time, build attribution list
          (save-excursion
            ;; Skip the current attribution,
            ;; which the user probably doesn't want to repeat.
            (re-search-backward tm-attribution-regexp nil 'move)
            ;; Search previously used attributions
            (setq tm-attributions nil)
            (while (re-search-backward tm-attribution-regexp nil 'move)
              (let ((attr (match-string 0)))
                (unless (member attr tm-attributions)
                  (push attr tm-attributions)))))
          (unless (bolp) (insert "\n\n"))
          (setq tm-attributions (nreverse tm-attributions))))
    (if tm-attributions
        ;; succeed
        (progn
          ;; remove the previous suggestion
          (kill-region (point-at-bol) (point-at-eol))
          (insert (pop tm-attributions))
          (setq this-command 'tm-insert-attribution))
      ;; fail
      (progn
        (message "No further characters above.")
        (setq this-command 'self-insert-command)))))

( defun tm-electric-linefeed()
  (interactive)
  (newline)
  (newline)
  (tm-insert-attribution)
  (insert " [")
  (tm-insert-timestamp)
  (insert "]")
  (newline)
)



(provide 'transcript)
;;; transcript.el ends here


