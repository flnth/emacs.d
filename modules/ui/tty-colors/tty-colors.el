;;; tty-colors.el                                      -*- lexical-binding: t -*-
;;
;; Filename: tty.el
;;
;; Copyright (C) 2018 Florian Thevißen
;;
;; Description: tty-emacs color configuration
;; Author: Florian Thevißen <mail@florian-thevissen.de>
;; Keywords: lisp, tools
;; Created: Sun Oct 28 17:15:36 2018 (+0100)
;; Version: 0.1.0
;; Package-Required: TODO
;; URL: TODO
;;
;; -----------------------------------------------------------------------------
;;; Commentary
;;
;; tty configuration:  colors
;;
;; -----------------------------------------------------------------------------
(message "loading tty-colors ...")

(defmacro +tty-colors--define-1 (n c r g b)
  `(tty-color-define ,n ,c (list (* 257 ,r) (* 257 ,g) (* 257 ,b))))

;;;###autoload
(defun +tty-colors-define-standard-colors ( )
  "Define the tty standard colors"
  (+tty-colors--define-1 "grey22" 16 55 55 55)
  (+tty-colors--define-1 "DarkSlateGrey" 17 55 55 95)
  (+tty-colors--define-1 "SlateBlue4" 18 55 55 135)
  (+tty-colors--define-1 "SlateBlue4" 19 55 55 175)
  (+tty-colors--define-1 "RoyalBlue3" 20 55 55 215)
  (+tty-colors--define-1 "RoyalBlue2" 21 55 55 255)
  (+tty-colors--define-1 "DarkSlateGrey" 22 55 95 55)
  (+tty-colors--define-1 "DarkSlateGrey" 23 55 95 95)
  (+tty-colors--define-1 "SteelBlue4" 24 55 95 135)
  (+tty-colors--define-1 "RoyalBlue3" 25 55 95 175)
  (+tty-colors--define-1 "RoyalBlue3" 26 55 95 215)
  (+tty-colors--define-1 "RoyalBlue2" 27 55 95 255)
  (+tty-colors--define-1 "ForestGreen" 28 55 135 55)
  (+tty-colors--define-1 "SeaGreen4" 29 55 135 95)
  (+tty-colors--define-1 "aquamarine4" 30 55 135 135)
  (+tty-colors--define-1 "SteelBlue" 31 55 135 175)
  (+tty-colors--define-1 "SteelBlue3" 32 55 135 215)
  (+tty-colors--define-1 "RoyalBlue1" 33 55 135 255)
  (+tty-colors--define-1 "LimeGreen" 34 55 175 55)
  (+tty-colors--define-1 "MediumSeaGreen" 35 55 175 95)
  (+tty-colors--define-1 "MediumSeaGreen" 36 55 175 135)
  (+tty-colors--define-1 "LightSeaGreen" 37 55 175 175)
  (+tty-colors--define-1 "SteelBlue3" 38 55 175 215)
  (+tty-colors--define-1 "DodgerBlue1" 39 55 175 255)
  (+tty-colors--define-1 "LimeGreen" 40 55 215 55)
  (+tty-colors--define-1 "SeaGreen3" 41 55 215 95)
  (+tty-colors--define-1 "SeaGreen3" 42 55 215 135)
  (+tty-colors--define-1 "MediumTurquoise" 43 55 215 175)
  (+tty-colors--define-1 "turquoise" 44 55 215 215)
  (+tty-colors--define-1 "turquoise" 45 55 215 255)
  (+tty-colors--define-1 "LimeGreen" 46 55 255 55)
  (+tty-colors--define-1 "SeaGreen2" 47 55 255 95)
  (+tty-colors--define-1 "SeaGreen2" 48 55 255 135)
  (+tty-colors--define-1 "SeaGreen1" 49 55 255 175)
  (+tty-colors--define-1 "turquoise" 50 55 255 215)
  (+tty-colors--define-1 "cyan1" 51 55 255 255)
  (+tty-colors--define-1 "grey27" 52 95 55 55)
  (+tty-colors--define-1 "grey32" 53 95 55 95)
  (+tty-colors--define-1 "MediumPurple4" 54 95 55 135)
  (+tty-colors--define-1 "MediumPurple4" 55 95 55 175)
  (+tty-colors--define-1 "purple3" 56 95 55 215)
  (+tty-colors--define-1 "BlueViolet" 57 95 55 255)
  (+tty-colors--define-1 "DarkOliveGreen" 58 95 95 55)
  (+tty-colors--define-1 "grey37" 59 95 95 95)
  (+tty-colors--define-1 "MediumPurple4" 60 95 95 135)
  (+tty-colors--define-1 "SlateBlue3" 61 95 95 175)
  (+tty-colors--define-1 "SlateBlue3" 62 95 95 215)
  (+tty-colors--define-1 "RoyalBlue1" 63 95 95 255)
  (+tty-colors--define-1 "DarkOliveGreen4" 64 95 135 55)
  (+tty-colors--define-1 "DarkSeaGreen4" 65 95 135 95)
  (+tty-colors--define-1 "PaleTurquoise4" 66 95 135 135)
  (+tty-colors--define-1 "SteelBlue" 67 95 135 175)
  (+tty-colors--define-1 "SteelBlue3" 68 95 135 215)
  (+tty-colors--define-1 "CornflowerBlue" 69 95 135 255)
  (+tty-colors--define-1 "DarkOliveGreen4" 70 95 175 55)
  (+tty-colors--define-1 "DarkSeaGreen4" 71 95 175 95)
  (+tty-colors--define-1 "CadetBlue" 72 95 175 135)
  (+tty-colors--define-1 "CadetBlue" 73 95 175 175)
  (+tty-colors--define-1 "SkyBlue3" 74 95 175 215)
  (+tty-colors--define-1 "SteelBlue1" 75 95 175 255)
  (+tty-colors--define-1 "LimeGreen" 76 95 215 55)
  (+tty-colors--define-1 "PaleGreen3" 77 95 215 95)
  (+tty-colors--define-1 "SeaGreen3" 78 95 215 135)
  (+tty-colors--define-1 "aquamarine3" 79 95 215 175)
  (+tty-colors--define-1 "MediumTurquoise" 80 95 215 215)
  (+tty-colors--define-1 "SteelBlue1" 81 95 215 255)
  (+tty-colors--define-1 "chartreuse2" 82 95 255 55)
  (+tty-colors--define-1 "SeaGreen2" 83 95 255 95)
  (+tty-colors--define-1 "SeaGreen1" 84 95 255 135)
  (+tty-colors--define-1 "SeaGreen1" 85 95 255 175)
  (+tty-colors--define-1 "aquamarine1" 86 95 255 215)
  (+tty-colors--define-1 "DarkSlateGray2" 87 95 255 255)
  (+tty-colors--define-1 "IndianRed4" 88 135 55 55)
  (+tty-colors--define-1 "HotPink4" 89 135 55 95)
  (+tty-colors--define-1 "MediumOrchid4" 90 135 55 135)
  (+tty-colors--define-1 "DarkOrchid" 91 135 55 175)
  (+tty-colors--define-1 "BlueViolet" 92 135 55 215)
  (+tty-colors--define-1 "purple1" 93 135 55 255)
  (+tty-colors--define-1 "tan4" 94 135 95 55)
  (+tty-colors--define-1 "LightPink4" 95 135 95 95)
  (+tty-colors--define-1 "plum4" 96 135 95 135)
  (+tty-colors--define-1 "MediumPurple3" 97 135 95 175)
  (+tty-colors--define-1 "MediumPurple3" 98 135 95 215)
  (+tty-colors--define-1 "SlateBlue1" 99 135 95 255)
  (+tty-colors--define-1 "LightGoldenrod4" 100 135 135 55)
  (+tty-colors--define-1 "wheat4" 101 135 135 95)
  (+tty-colors--define-1 "grey53" 102 135 135 135)
  (+tty-colors--define-1 "LightSlateGrey" 103 135 135 175)
  (+tty-colors--define-1 "MediumPurple" 104 135 135 215)
  (+tty-colors--define-1 "LightSlateBlue" 105 135 135 255)
  (+tty-colors--define-1 "OliveDrab3" 106 135 175 55)
  (+tty-colors--define-1 "DarkOliveGreen3" 107 135 175 95)
  (+tty-colors--define-1 "DarkSeaGreen" 108 135 175 135)
  (+tty-colors--define-1 "LightSkyBlue3" 109 135 175 175)
  (+tty-colors--define-1 "LightSkyBlue3" 110 135 175 215)
  (+tty-colors--define-1 "SkyBlue2" 111 135 175 255)
  (+tty-colors--define-1 "OliveDrab3" 112 135 215 55)
  (+tty-colors--define-1 "DarkOliveGreen3" 113 135 215 95)
  (+tty-colors--define-1 "PaleGreen3" 114 135 215 135)
  (+tty-colors--define-1 "DarkSeaGreen3" 115 135 215 175)
  (+tty-colors--define-1 "DarkSlateGray3" 116 135 215 215)
  (+tty-colors--define-1 "SkyBlue1" 117 135 215 255)
  (+tty-colors--define-1 "GreenYellow" 118 135 255 55)
  (+tty-colors--define-1 "LightGreen" 119 135 255 95)
  (+tty-colors--define-1 "LightGreen" 120 135 255 135)
  (+tty-colors--define-1 "PaleGreen1" 121 135 255 175)
  (+tty-colors--define-1 "aquamarine1" 122 135 255 215)
  (+tty-colors--define-1 "DarkSlateGray1" 123 135 255 255)
  (+tty-colors--define-1 "brown" 124 175 55 55)
  (+tty-colors--define-1 "maroon" 125 175 55 95)
  (+tty-colors--define-1 "VioletRed3" 126 175 55 135)
  (+tty-colors--define-1 "DarkOrchid" 127 175 55 175)
  (+tty-colors--define-1 "DarkOrchid2" 128 175 55 215)
  (+tty-colors--define-1 "DarkOrchid1" 129 175 55 255)
  (+tty-colors--define-1 "sienna" 130 175 95 55)
  (+tty-colors--define-1 "IndianRed" 131 175 95 95)
  (+tty-colors--define-1 "HotPink3" 132 175 95 135)
  (+tty-colors--define-1 "MediumOrchid3" 133 175 95 175)
  (+tty-colors--define-1 "MediumOrchid" 134 175 95 215)
  (+tty-colors--define-1 "MediumPurple2" 135 175 95 255)
  (+tty-colors--define-1 "tan3" 136 175 135 55)
  (+tty-colors--define-1 "LightSalmon3" 137 175 135 95)
  (+tty-colors--define-1 "RosyBrown" 138 175 135 135)
  (+tty-colors--define-1 "grey63" 139 175 135 175)
  (+tty-colors--define-1 "MediumPurple2" 140 175 135 215)
  (+tty-colors--define-1 "MediumPurple1" 141 175 135 255)
  (+tty-colors--define-1 "OliveDrab3" 142 175 175 55)
  (+tty-colors--define-1 "DarkKhaki" 143 175 175 95)
  (+tty-colors--define-1 "NavajoWhite3" 144 175 175 135)
  (+tty-colors--define-1 "grey69" 145 175 175 175)
  (+tty-colors--define-1 "LightSteelBlue3" 146 175 175 215)
  (+tty-colors--define-1 "LightSteelBlue" 147 175 175 255)
  (+tty-colors--define-1 "OliveDrab2" 148 175 215 55)
  (+tty-colors--define-1 "DarkOliveGreen3" 149 175 215 95)
  (+tty-colors--define-1 "DarkSeaGreen3" 150 175 215 135)
  (+tty-colors--define-1 "DarkSeaGreen2" 151 175 215 175)
  (+tty-colors--define-1 "LightCyan3" 152 175 215 215)
  (+tty-colors--define-1 "LightSkyBlue1" 153 175 215 255)
  (+tty-colors--define-1 "GreenYellow" 154 175 255 55)
  (+tty-colors--define-1 "DarkOliveGreen2" 155 175 255 95)
  (+tty-colors--define-1 "PaleGreen1" 156 175 255 135)
  (+tty-colors--define-1 "DarkSeaGreen2" 157 175 255 175)
  (+tty-colors--define-1 "DarkSeaGreen1" 158 175 255 215)
  (+tty-colors--define-1 "PaleTurquoise1" 159 175 255 255)
  (+tty-colors--define-1 "brown3" 160 215 55 55)
  (+tty-colors--define-1 "VioletRed3" 161 215 55 95)
  (+tty-colors--define-1 "VioletRed3" 162 215 55 135)
  (+tty-colors--define-1 "maroon2" 163 215 55 175)
  (+tty-colors--define-1 "MediumOrchid" 164 215 55 215)
  (+tty-colors--define-1 "DarkOrchid1" 165 215 55 255)
  (+tty-colors--define-1 "sienna3" 166 215 95 55)
  (+tty-colors--define-1 "IndianRed" 167 215 95 95)
  (+tty-colors--define-1 "HotPink3" 168 215 95 135)
  (+tty-colors--define-1 "HotPink2" 169 215 95 175)
  (+tty-colors--define-1 "orchid" 170 215 95 215)
  (+tty-colors--define-1 "MediumOrchid1" 171 215 95 255)
  (+tty-colors--define-1 "tan3" 172 215 135 55)
  (+tty-colors--define-1 "LightSalmon3" 173 215 135 95)
  (+tty-colors--define-1 "LightPink3" 174 215 135 135)
  (+tty-colors--define-1 "pink3" 175 215 135 175)
  (+tty-colors--define-1 "plum3" 176 215 135 215)
  (+tty-colors--define-1 "violet" 177 215 135 255)
  (+tty-colors--define-1 "goldenrod" 178 215 175 55)
  (+tty-colors--define-1 "LightGoldenrod3" 179 215 175 95)
  (+tty-colors--define-1 "tan" 180 215 175 135)
  (+tty-colors--define-1 "MistyRose3" 181 215 175 175)
  (+tty-colors--define-1 "thistle3" 182 215 175 215)
  (+tty-colors--define-1 "plum2" 183 215 175 255)
  (+tty-colors--define-1 "OliveDrab2" 184 215 215 55)
  (+tty-colors--define-1 "khaki3" 185 215 215 95)
  (+tty-colors--define-1 "LightGoldenrod2" 186 215 215 135)
  (+tty-colors--define-1 "LightYellow3" 187 215 215 175)
  (+tty-colors--define-1 "grey84" 188 215 215 215)
  (+tty-colors--define-1 "LightSteelBlue1" 189 215 215 255)
  (+tty-colors--define-1 "OliveDrab1" 190 215 255 55)
  (+tty-colors--define-1 "DarkOliveGreen1" 191 215 255 95)
  (+tty-colors--define-1 "DarkOliveGreen1" 192 215 255 135)
  (+tty-colors--define-1 "DarkSeaGreen1" 193 215 255 175)
  (+tty-colors--define-1 "honeydew2" 194 215 255 215)
  (+tty-colors--define-1 "LightCyan1" 195 215 255 255)
  (+tty-colors--define-1 "firebrick1" 196 255 55 55)
  (+tty-colors--define-1 "brown1" 197 255 55 95)
  (+tty-colors--define-1 "VioletRed1" 198 255 55 135)
  (+tty-colors--define-1 "maroon1" 199 255 55 175)
  (+tty-colors--define-1 "maroon1" 200 255 55 215)
  (+tty-colors--define-1 "magenta1" 201 255 55 255)
  (+tty-colors--define-1 "tomato1" 202 255 95 55)
  (+tty-colors--define-1 "IndianRed1" 203 255 95 95)
  (+tty-colors--define-1 "IndianRed1" 204 255 95 135)
  (+tty-colors--define-1 "HotPink" 205 255 95 175)
  (+tty-colors--define-1 "HotPink" 206 255 95 215)
  (+tty-colors--define-1 "MediumOrchid1" 207 255 95 255)
  (+tty-colors--define-1 "sienna1" 208 255 135 55)
  (+tty-colors--define-1 "salmon1" 209 255 135 95)
  (+tty-colors--define-1 "LightCoral" 210 255 135 135)
  (+tty-colors--define-1 "PaleVioletRed1" 211 255 135 175)
  (+tty-colors--define-1 "orchid2" 212 255 135 215)
  (+tty-colors--define-1 "orchid1" 213 255 135 255)
  (+tty-colors--define-1 "goldenrod1" 214 255 175 55)
  (+tty-colors--define-1 "SandyBrown" 215 255 175 95)
  (+tty-colors--define-1 "LightSalmon1" 216 255 175 135)
  (+tty-colors--define-1 "LightPink1" 217 255 175 175)
  (+tty-colors--define-1 "pink1" 218 255 175 215)
  (+tty-colors--define-1 "plum1" 219 255 175 255)
  (+tty-colors--define-1 "goldenrod1" 220 255 215 55)
  (+tty-colors--define-1 "LightGoldenrod2" 221 255 215 95)
  (+tty-colors--define-1 "LightGoldenrod2" 222 255 215 135)
  (+tty-colors--define-1 "NavajoWhite1" 223 255 215 175)
  (+tty-colors--define-1 "MistyRose1" 224 255 215 215)
  (+tty-colors--define-1 "thistle1" 225 255 215 255)
  (+tty-colors--define-1 "yellow1" 226 255 255 55)
  (+tty-colors--define-1 "LightGoldenrod1" 227 255 255 95)
  (+tty-colors--define-1 "khaki1" 228 255 255 135)
  (+tty-colors--define-1 "wheat1" 229 255 255 175)
  (+tty-colors--define-1 "cornsilk1" 230 255 255 215)
  (+tty-colors--define-1 "grey100" 231 255 255 255)
  (+tty-colors--define-1 "grey3" 232 8 8 8)
  (+tty-colors--define-1 "grey7" 233 18 18 18)
  (+tty-colors--define-1 "grey11" 234 28 28 28)
  (+tty-colors--define-1 "grey15" 235 38 38 38)
  (+tty-colors--define-1 "grey19" 236 48 48 48)
  (+tty-colors--define-1 "grey23" 237 58 58 58)
  (+tty-colors--define-1 "grey27" 238 68 68 68)
  (+tty-colors--define-1 "grey31" 239 78 78 78)
  (+tty-colors--define-1 "grey35" 240 88 88 88)
  (+tty-colors--define-1 "grey39" 241 98 98 98)
  (+tty-colors--define-1 "grey42" 242 108 108 108)
  (+tty-colors--define-1 "grey46" 243 118 118 118)
  (+tty-colors--define-1 "grey50" 244 128 128 128)
  (+tty-colors--define-1 "grey54" 245 138 138 138)
  (+tty-colors--define-1 "grey58" 246 148 148 148)
  (+tty-colors--define-1 "grey62" 247 158 158 158)
  (+tty-colors--define-1 "grey66" 248 168 168 168)
  (+tty-colors--define-1 "grey70" 249 178 178 178)
  (+tty-colors--define-1 "grey74" 250 188 188 188)
  (+tty-colors--define-1 "grey78" 251 198 198 198)
  (+tty-colors--define-1 "grey82" 252 208 208 208)
  (+tty-colors--define-1 "grey86" 253 218 218 218)
  (+tty-colors--define-1 "grey90" 254 228 228 228)
  (+tty-colors--define-1 "grey93" 255 238 238 238))

;;;###autoload
(defun +tty-colors-define-modified-colors()
  "Define the 16 standard colors the terminal actually uses."
  (+tty-colors--define-1  "black"              0  42  40  40 )
  (+tty-colors--define-1  "red"                1  172 68  63 )
  (+tty-colors--define-1  "green"              2  131 167 72 )
  (+tty-colors--define-1  "yellow"             3  207 124 67 )
  (+tty-colors--define-1  "blue"               4  69  77  208)
  (+tty-colors--define-1  "magenta"            5  110 108 203)
  (+tty-colors--define-1  "cyan"               6  54  141 130)
  (+tty-colors--define-1  "white"              7  198 171 148)
  (+tty-colors--define-1  "brightblack"        8  62  66  73 )
  (+tty-colors--define-1  "brightred"          9  204 102 102)
  (+tty-colors--define-1  "brightgreen"        10 152 189 94  )
  (+tty-colors--define-1  "brightyellow"       11 251 203 65  )
  (+tty-colors--define-1  "brightblue"         12 51  126 190 )
  (+tty-colors--define-1  "brightmagenta"      13 140 138 194 )
  (+tty-colors--define-1  "brightcyan"         14 109 190 179 )
  (+tty-colors--define-1  "brightwhite"        15 223 207 193 )
  (+tty-colors--define-1  "color-234"          234 29  29  26 )	;; terminal background
  (+tty-colors--define-1  "color-235"          235 33  33  33 )	;; current line background
  )

(defun +tty-colors-fix-tty-colors ()
  (interactive)
  (setq ff '())
  (cl-loop for frame in (frame-list) do
		   (when (not (display-graphic-p frame))
			 (let ((current-frame (selected-frame)))
				 (add-to-list 'ff frame)
				 (select-frame frame t)
				 (tty-run-terminal-initialization frame)
				 (+tty-colors-define-standard-colors)
				 (+tty-colors-define-modified-colors)
				 (select-frame current-frame t)))))

;;; tools for tty/gui color management (unused)

;;; 
(defun +tty-colors--unused ()
  ;; ----------
  (defun fn-face-has-tty-x-config (face)
	(let ((spec (get face 'face-override-spec))
		  has-x has-tty)
	  (cl-loop for entry in spec do
			   (let ((type (ignore-errors (cadaar entry))))
				 (cond ((eq type 'tty)
						(setq has-tty t)))
				 (cond ((eq type 'x)
						(setq has-x t)))))
	  (and has-x has-tty)))

  (defun fn-hex-color? (color-string)
	"Returns color-string if it is in hex format, nil otherwise."
	(ignore-errors
	  (when (s-prefix? "#" color-string) color-string))
	)

  (defmacro not-null (body)
	`(not (null ,body)))

  (defun fn-convert-hex-to-tty (color-hex-string)
	"Converts color-hex-string to tty-equivalent string. Returns
nil if failed."
	(ignore-errors
	  (let* ((r (string-to-number (substring color-string 1 3) 16))
			 (g (string-to-number (substring color-string 3 5) 16))
			 (b (string-to-number (substring color-string 5 7) 16)))
		(car (tty-color-approximate (list (* r 256) (* g 256) (* b 256)))))))

  (defun fn-convert-tty-color-to-hex (color)
	"Converts a tty color e.g. color-234 into hex code. Returns nil
if failed."
	(ignore-errors
	  (concat "#"
			  (mapconcat 'identity (mapcar (lambda (x) (subst-char-in-string ? ?0 (format "%2x" (/ x 256))) ) (color-values color) ) "" )))
	)

  (defun fn-translate-colors-tty (lst)
	"Replaces color-strings with their tty equivalents in passed
  plist. Currently acts on :foreground and :background."
	(let ((color-props '(:foreground :background))
		  (lst-copy (copy-list lst)))
	  (cl-loop for p in color-props do
			   (let ((color-string (plist-get lst-copy p)))
				 (cond ((fn-hex-color? color-string) ; hex color string
						(plist-put lst-copy p (fn-convert-hex-to-tty color-string)))
					   ((not-null color-string)	; non-hex color string
						(plist-put lst-copy p (concat "color-"
					  								  (number-to-string (tty-color-translate color-string))))
						)
					   )
				 ))
	  lst-copy))


  (defun fn-translate-colors-x (lst)
	"Replaces non-x-compatible color-specs with their x-equivalents
  in passed plist. Currently acts on :foreground and
  :background."
	(let ((color-props '(:foreground :background ))
		  (lst-copy (copy-list lst)))
	  (cl-loop for p in color-props do
			   (let ((color-string (plist-get lst-copy p)))
				 (when (and (not (fn-hex-color? color-string))
							(not (-contains? (x-defined-colors) color-string)))
				   (plist-put lst-copy p (fn-convert-tty-color-to-hex color-string)))))
	  lst-copy))

  (defun fn-split-face-color (face)
	"Split face specification in-two with regards to color; tty and x-specific."
	(let* ((foreground (face-attribute face :foreground))
		   (background (face-attribute face :background))
		   (face-spec (or (get face 'face-override-spec)
						  (cadr (assoc 'user (get face 'theme-face)))))
		   (spec (cadar face-spec)))

	  (if (and (not-null spec)
			   (not (fn-face-has-tty-x-config face))
			   (or (not-null foreground)
				   (not-null background)))
		  `(
			(((type x)) ,spec) ; insert old spec, (TODO: but ensure x-compatibility?)
			(((type tty)) ,(fn-translate-colors-tty spec)) ; translate all colors
			)
		face-spec)))

  (defun fn-ensure-face-x-compatible (face)
	"Ensure x-compatibility for passed face. Return face
specification."
	(let ((face-spec (or (get face 'face-spec)
						 (cadr (assoc 'user (get face 'theme-face))))))
	  (cl-loop for type-spec in face-spec do
			   (when (eq (cadaar type-spec) 'x)
				 (setf (nth 1 type-spec) (fn-translate-colors-x (cadr type-spec)))
				 ))
	  (when (eq (cadaar face-spec) 'x)
		(face-spec-set face (fn-translate-colors-x face-spec)))
	  face-spec))

  (defun fn-split-face-colors-tty-x ()
	""
	(interactive)
	(cl-loop for face in (face-list) do
			 (face-spec-set face (fn-split-face-color face))))

  ;; -- dev code ------
  ;; (insert (format "%s" (symbol-plist 'default)))
  ;; types of face specs:
  ;;   - saved-face:   spec saved by user using customization buffer
  ;;   - customized-face:  face-spec customized for current session but unsaved
  ;;   - theme-face:   active customization settings and cusotm themes with face specs for that face

  ;; ....question:  theme-face does what, where?

  (defun fn-face-get-x-spec (face )
	"Returns the face spec that would be used for X frames."
	;; ...can't do that. Is counter to the way the stuff is organized internally:
	)

  (defun fn-face-get-tty-spec (face)
	"Returns the face spec that would be sued for tty frames "
	;; ...can't do that.
	)
  (defun fn-face-has-x-incopatible-color-spec? (face)
	"Returns t if face has an incompatible x-spec."

	;; for every spec:
	;; check non non type-specs
	;; check x-specs

	)

  ;; (defun fn-faces-find-problems ()
  ;;   "Search through all faces, identify problems."
  ;;   (cl-loop for face in (face-list) do
  ;; 		   ;; TODO: x face has non-x compatible spec   (not in x-color list AND no hex color)

  ;; 		   )

  ;;   ;; TODO: tty-face is non-tty compatible     (== in format color-XXX)
  ;; )


  (defun fn-faces-ensure-x-compatibility ()
	"Ensure `face-override-spec' face specifications are x-compatible."
	(cl-loop for face in (face-list) do
			 ;; TODO: doesnt work. fix.
			 (face-spec-set face (fn-ensure-face-x-compatible face))
			 )
	))


(message "... done!")
(provide 'ui/tty-colors/tty-colors)
