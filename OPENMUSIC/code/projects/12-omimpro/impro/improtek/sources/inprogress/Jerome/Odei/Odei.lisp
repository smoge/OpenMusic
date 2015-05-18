(in-package :om)

(setf path_dir "/Users/jnika/Desktop/odei_16_02_preparation/TEST/"
      name_metrotrack "metro"
      list-name_track (list "track1" "track2" name_metrotrack)
      tempo 126)

(align-tracklist-on-tempo-grid path_dir list-name_track name_metrotrack tempo)


#|
path_dir (chemin dossier) et noms fichiers : sans espaces ni accents, "/" à la fin de path_dir
----------------------------------------------------------------------------------------------
cmd+Y pour évaluer
pistes sauvegardées dans le dossier sous le nom "...MODIF.mid"

Ex :
----
(setf path_dir "/Users/jnika/Desktop/odei_16_02_preparation/TEST/"
      name_metrotrack "metro"
      list-name_track (list "track1" "track2" name_metrotrack)
      tempo 126)
|#