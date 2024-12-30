# Jaguar Libraries

This repository contains various libraries and other bits of code that can make
programming the Atari Jaguar easier.

## SPU Library v1.05b by Robert Jurziga

The Jaguar Sound System is a sample replay system for the Atari Jaguar 64 bit
system. The core of the Sound System is the SPU library, that offers functions
to produce sound.

**SPU features**
- max. 12 voices
- user defined voice number processed by the DSP. (from 1 to 12 voices)
- six main mixing frequencies (8kHz, 11kHz, 16kHz, 22kHz, 32kHz, 44.1kHz)
- dynamic voice allocation
- notes from C-1 to B-7
- different sample loop types (forward, backward, and pingpong)
- arbitary starting point and range within the sample size
- individual voice volume, pan, and pitch
- modification of voice parameters during runtime
- custom file format for samples (JSP!)
- limited support for standard sample foramts like WAV, AIFC, AIFF, RAW

## CDROM Library v1.0b by Robert Jurziga

The Jaguar CDROM library is responsable for interaction with the Jaguar CDROM's
CDDA functionallity, by sending commands. Loading data from a CD isn't currently
supported. Due to insufficient informations on the CDROM unit, the library does
not cover the whole range of commands and possibility the CDROM unit offers.
 
It is possible to use this library as a standalone library, but due to the nature
of Jaguar's hardware no audible results will be produced as long as CDDA data
stream isn't channeled to the Jaguar's DACs by the DSP. SPU implements this
functionallity within its DSP core, which additionally makes it possible to mix
the CDDA data stream with sample playback. JCD can bee seen as a subset of the
Jaguar Sound System.

