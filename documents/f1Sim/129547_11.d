format 67

subject 128011 ""
  xyzwh 100 77 2000 393 277
usecasecanvas 128139 usecase_ref 131339 // Ottieni dati
  xyzwh 365 145 3005 111 34 label_xy 396 153
end
usecasecanvas 128267 usecase_ref 131467 // ConcavitÓ
  xyzwh 179 300 3005 94 38 label_xy 200 311
end
usecasecanvas 128395 usecase_ref 131595 // Ampiezza
  xyzwh 351 298 3005 82 42 label_xy 365 310
end
usecasecanvas 128523 usecase_ref 131723 // Lunghezza
  xyzwh 138 217 3005 93 34 label_xy 158 226
end
usecasecanvas 128651 usecase_ref 131851 // Curvatura
  xyzwh 304 218 3005 96 44 label_xy 328 230
end
note 129291 "Traiettoria"
  xyzwh 217 29 2005 84 35
classcanvas 129419 class_ref 129547 // Tratto
  class_drawing_mode default show_context_mode default show_stereotype_properties default
  xyz 22 129 2005
end
usecasecanvas 129803 usecase_ref 133259 // Imposta caratteristiche traiettoria
  xyzwh 135 88 3005 188 50 label_xy 152 104
end
simplerelationcanvas 128779 simplerelation_ref 130187
  from ref 128651 z 3004 stereotype "<<include>>" xyz 251.5 279.5 3000 to ref 128267
end
simplerelationcanvas 128907 simplerelation_ref 130315
  from ref 128651 z 3004 stereotype "<<include>>" xyz 341.5 278.5 3000 to ref 128395
end
simplerelationcanvas 129035 simplerelation_ref 130443
  from ref 128139 z 3004 stereotype "<<include>>" xyz 267 195.5 3000 to ref 128523
end
simplerelationcanvas 129163 simplerelation_ref 130571
  from ref 128139 z 3004 stereotype "<<include>>" xyz 354 196 3000 to ref 128651
end
line 129675 --->
  from ref 129419 z 2004 to ref 128139
line 129931 --->
  from ref 129419 z 2004 to ref 129803
end
