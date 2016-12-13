Transcript
==========

Transcribing interviews can be tedious work. This script goes someway to making the process easier. 

Background
----------

If you have to do your own transcription, a ratio of 1:4 or even 1:6 is not unusual (unless you an experienced typist). The easiest way to do it more quickly is by using some form of voice recognition software.

Modern voice recognition software can be quite quickly trained to recognise one voice. Alas, recognising multiple voices is often just too hard.

So often the best way to transcribe something is to listen to the recordings and then speak them back to the recognition software. I have done this quite a bit, and it is easy to get close to a 1:2 or even 1:1.5 transcription ration; i.e., one hour of recordings can be transcribed in 2 hours. 

Software
--------

What is needed is some software to control the playback of the recordings. That's what this the transcript.el script is for. It allows you to control mplayer to play back the recording.

Set-up
------

The file to which you are writing the transcription needs a special header to tell it which audio file it is working with. 

	<!-- 
	Local Variables:
	mode: transcript
	tm-sound-file: "/home/psmith/NAS/Work/Research/Data/Recordings/AWT/T 20060220 1325 Peter Stevens2nd.wav"
	tm-playback-position: 0
	tm-point: 0
	End:
	-->

Keys
----

If you look at the header of the script, you will see all the ways you might set up the keys to control the transcription process. But the default comes like this:

- The `insert` key starts and stops the playback. 
- The `delete` key goes back. 
- The `ctl-insert` key inserts a time stamp in the file so you can do things like:

    00:02:20 Interviewer: And so I said ..
	00:02:23 Respondent: What?
	
Of course you can use abbreviations to make your life so much easier.
