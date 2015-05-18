;;;;============================================================================
;;;;               Improtek.lib
;;;;
;;;; Improtek library (formerly named OMax 2.0)
;;;; authors: G. Assayag, M. Chemillier, J. Nika
;;;; date: 2012 (2004 for the OMax 2.0 version)
;;;;============================================================================ 

(in-package :om)

;;;;============================================================================
#|
ImproteK requires the following external libraries : 
- Repmus (in the folder "libraries" of the OpenMusic distribution : http://repmus.ircam.fr/openmusic/download)
- Bordeaux-threads (http://common-lisp.net/project/bordeaux-threads/) and its dependencies 
Alexandria (http://common-lisp.net/project/alexandria/) and Fiveam (http://common-lisp.net/project/fiveam/)


A folder containing all the ImproteK dependencies can be downloaded at:
https://drive.google.com/folderview?id=0B-b9cibNADQ0dl96U3BWVUZvVjg&usp=sharing


If ImproteK is used within the OpenMusic environment, these libraries have to be loaded with OpenMusic, 
or their source files have to be located in the folder of your external libraries whose path 
is specified in the OpenMusic preferences.
|#

(require-library "RepMus")
(require-library "bordeaux-threads")
;;;;============================================================================


(defvar *improtek-source-dir* nil)
(setf *improtek-source-dir* (append (pathname-directory *load-pathname*) (list "sources")))


(defvar *improtek-lib-files* nil)
(setf *improtek-lib-files* '(
                             ;"1-Load-Init/cleansources"
                             "1-Load-Init/WorkDirectory"
                             "1-Load-Init/WorkDirectory_Paths"

                             "2-GenerationProcesses/Oracle"
                             "2-GenerationProcesses/Label-Data-Event"
                             "2-GenerationProcesses/PrefixIndexing"
                             "2-GenerationProcesses/Improvizer"
                             
                             "3-EventSubclasses/MidiHarmBeat/HarmLabel-HarmBeat"  
                             "3-EventSubclasses/MidiHarmBeat/MidiHarmBeat" 
                             "3-EventSubclasses/MidiHarmBeat/Midifile_tools" 
                             "3-EventSubclasses/MidiHarmBeat/Midiharmbeatlist_tools"
                             "3-EventSubclasses/AudioBeat/AudioHarmBeat"
                             "3-EventSubclasses/AudioBeat/AudioDescrBeat"
                             "3-EventSubclasses/AudioBeat/Audiobeat_tools"
                             
                             ;"3-EventSubclasses/Melobeatlist"
                             
                             "6-Data/Scenarios/MidiHarmBeat/ImprotekData"
                             "Others/HarmoArrang/Substitution" ;

                             "4-Save-Send-Receive/Antescofo"
                             "4-Save-Send-Receive/Save-Format-Send"


                             "5-HandleImpro/Tune"
                             "5-HandleImpro/RealTimeTools/AsynchEventDrivenProdCons"
                             "5-HandleImpro/RealTimeImprovizer"
                             "5-HandleImpro/RealTimeTools/MidiToolsForRealTime"
                             "5-HandleImpro/RealTimeTune"
                            
                            
                             ;"Others/Styles/Hermeto";
                             "6-Data/Scenarios/MidiHarmBeat/GridData1"
                             "6-Data/Scenarios/MidiHarmBeat/GridData2"
                             "6-Data/Scenarios/AudioDescr/Data_AudioRmsCentroidBeat1"
                             "6-Data/Scenarios/AudioDescr/Data_AudioRmsCentroidBeat2"
                             "6-Data/Scenarios/AudioDescr/Data_AudioRmsPitch"
                             "6-Data/Scenarios/AudioDescr/Data_AudioDensityCentroid"
                             "6-Data/Scenarios/AudioDescr/Data_AudioCentroidRoughness"
                             "6-Data/Scenarios/AudioDescr/CoupleDescrAudio"
                             "6-Data/Memories/HackOldData"

                             "4-Save-Send-Receive/HandleReceivedOSC"
                             ))


(mapc #'(lambda (file) (compile&load (make-pathname :directory *improtek-source-dir* :name file))) *improtek-lib-files*)



