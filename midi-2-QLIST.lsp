
(midi2qlist "/Volumes/Documents/Dropbox/Proyecto_Trombon/MIDI/Q2M-final-org-NOREPEAT.mid"
            "/Volumes/Documents/Dropbox/Proyecto_Trombon/MIDI/Q2M-final-org-NOREPEAT.txt" 1)

(defun midi2qlist (midi-file qlist-file &rest tracks)
  (let* ((events (if tracks
                     (loop for tr in tracks appending
                          (midi-file-to-events midi-file :track tr))
                     (midi-file-to-events midi-file))))
    ;; there might be some nil events so remove them then intermingle the
    ;; tracks, sorting by start time 
    (setq events (sort (remove-if #'not events)
                       #'(lambda (x y)
                           (< (start-time x) (start-time y)))))
    (with-open-file (qlist qlist-file :direction :output
                           :if-does-not-exist :create
                           :if-exists :overwrite)
      (loop for e in events with last-time = 0.0 do
           ;; qlist line format is the delay before the message is sent; the
           ;; receiver; the data; a semi-colon at the end of the line
           (format qlist "~&~,3f qlmidinote ~a ~a;"
                   ;; delay is in millisecs
                   (* 1000.0 (-  (start-time e) last-time))
                   (midi-note (pitch-or-chord e))
                   (floor (* 127 (amplitude e))))
           (setf last-time (start-time e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


