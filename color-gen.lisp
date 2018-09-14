(in-package :color-gen)


(defvar *colors*
  '(black     ("#4E4E4E"     "#7C7C7C")
    red       ("#FF6C60"     "#FFB6B0")
    green     ("#A8FF60"     "#CEFFAB")
    yellow    ("#FFFFB6"     "#FFFFCB")
    blue      ("#96CBFE"     "#B5DCFE")
    magenta   ("#FF73FD"     "#FF9CFE")
    cyan      ("#C6C5FE"     "#DFDFFE")
    white     ("#EEEEEE"     "#FFFFFF")
    foreground ("#f6f3e8")
    background ("#000000")))

(defconstant +colors+
  '(black red green yellow blue magenta cyan white))

(defmacro with-colors (colors &rest body)
  (let ((color-names (loop for color in +colors+
                        collect `(,color (car (getf ,colors ',color)))
                        collect `(,(symbolicate color "-BRIGHT") (cadr (getf ,colors ',color)))))
        (foreground-name `((foreground (car (getf ,colors 'foreground)))))
        (background-name `((background (car (getf ,colors 'background))))))
    `(let ,(concatenate 'list color-names foreground-name background-name)
       (declare (ignorable
                 black red green yellow blue magenta cyan white
                 black-bright red-bright green-bright yellow-bright
                 blue-bright magenta-bright cyan-bright white-bright
                 foreground background))
       ,@body)))


(defvar *indentation-level* 2)

(defun writeln (indent &rest strings)
  (loop repeat (* indent *indentation-level*) do (write-string " "))
  (loop for x in strings
     do (write-string (format nil  "~a" x)))
  (write-line "")
  (values))


  

(defun alacritty-colors (colors)
  (with-colors colors
    (let ((indent 0))
      (macrolet ((out (name &optional (color name))
                   `(writeln indent
                             ,(format nil "~(~a~)" name)
                             ": '0x" (format nil "~(~a~)" (subseq ,color 1)) "'")))

        ("colors"
         ("primary"
          (background)
          (foreground))
         ("cursor"
          ("text" background)
          ("cursor" foreground))
         ("normal"
          (black)
          (red)
          (green)
          (yellow)
          (blue)
          (magenta)
          (cyan)
          (white))
         ("bright"
          ("black" black-bright)
          ("red" red-bright)
          ("green" green-bright)
          ("yellow" yellow-bright)
          ("blue" blue-bright)
          ("magenta" magenta-bright)
          ("cyan" cyan-bright)
          ("white" white-bright)))

        
        (writeln indent "colors:")
        (incf indent)

        (writeln indent "primary:")
        (incf indent)
        (out background)
        (out foreground)
        (decf indent)


        (writeln indent "cursor:")
        (incf indent)
        (out text background)
        (out cursor foreground)
        (decf indent)

        (writeln indent "normal:")
        (incf indent)
        (out black)
        (out red)
        (out green)
        (out yellow)
        (out blue)
        (out magenta)
        (out cyan)
        (out white)
        (decf indent)

        (writeln indent "bright:")
        (incf indent)
        (out black black-bright)
        (out red red-bright)
        (out green green-bright)
        (out yellow yellow-bright)
        (out blue blue-bright)
        (out magenta magenta-bright)
        (out cyan cyan-bright)
        (out white white-bright)))))

(alacritty-colors *colors*)

