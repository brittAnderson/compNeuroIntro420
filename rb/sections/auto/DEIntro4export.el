;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "DEIntro4export"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("graphicx" "") ("hyperref" "") ("relsize" "") ("amsmath" "") ("mathabx" "") ("wasysym" "nointegrals") ("newtxmath" "") ("textcomp" "") ("framed" "") ("hyphenat" "htt") ("color" "usenames" "dvipsnames") ("CJK" "") ("savesym" "") ("amsfonts" "") ("amsthm" "")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "book"
    "bk10"
    "inputenc"
    "fontenc"
    "graphicx"
    "hyperref"
    "relsize"
    "amsmath"
    "mathabx"
    "wasysym"
    "newtxmath"
    "textcomp"
    "framed"
    "hyphenat"
    "color"
    "CJK"
    "savesym"
    "amsfonts"
    "amsthm")
   (TeX-add-symbols
    '("Ssubsubsubsubsectiongrouperstar" 1)
    '("Ssubsubsubsectiongrouperstar" 1)
    '("Ssubsubsectiongrouperstar" 1)
    '("Ssubsectiongrouperstar" 1)
    '("Ssubsubsubsubsectiongrouper" 2)
    '("Ssubsubsubsectiongrouper" 2)
    '("Ssubsubsectiongrouper" 2)
    '("Ssubsectiongrouper" 2)
    '("Ssubsubsubsubsectionstarx" 2)
    '("Ssubsubsubsectionstarx" 2)
    '("Ssubsubsectionstarx" 2)
    '("Ssubsectionstarx" 2)
    '("Ssectionstarx" 2)
    '("Spartstarx" 2)
    '("Ssubsubsubsubsectionstar" 1)
    '("Ssubsubsubsectionstar" 1)
    '("Ssubsubsectionstar" 1)
    '("Ssubsectionstar" 1)
    '("Ssectionstar" 1)
    '("Spartstar" 1)
    '("Ssubsubsubsubsection" 2)
    '("Ssubsubsubsection" 2)
    '("Ssubsubsection" 2)
    '("Ssubsection" 2)
    '("Ssection" 2)
    '("Spart" 2)
    '("ChapRefUC" 2)
    '("ChapRef" 2)
    '("Iidentity" 1)
    '("RpackageSpec" 1)
    '("RBackgroundLabel" 1)
    '("RfilecontentBox" 1)
    '("Rfilename" 1)
    '("RfiletitleBox" 1)
    '("Rfiletitle" 1)
    '("RfileboxBoxB" 2)
    '("RfileboxBoxC" 2)
    '("RfileboxBoxT" 2)
    '("RfileboxBox" 3)
    '("Rfilebox" 2)
    '("bibentry" 1)
    '("highlighted" 1)
    '("RktIn" 1)
    '("RktOpt" 1)
    '("RktErr" 1)
    '("RktErrCol" 1)
    '("RktVar" 1)
    '("RktVarCol" 1)
    '("RktRdr" 1)
    '("RktMod" 1)
    '("RktMeta" 1)
    '("RktOut" 1)
    '("RktRes" 1)
    '("RktModLink" 1)
    '("RktValDef" 1)
    '("RktValLink" 1)
    '("RktVal" 1)
    '("RktSymDef" 1)
    '("RktSym" 1)
    '("RktInBG" 1)
    '("RktPn" 1)
    '("RktCmt" 1)
    '("RktStxDef" 1)
    '("RktStxLink" 1)
    '("RktKw" 1)
    '("RktPlain" 1)
    '("inColor" 2)
    '("SHyphen" 1)
    '("SColorize" 2)
    '("SHistory" 1)
    '("Snolinkurl" 1)
    '("Shref" 3)
    '("Svcenter" 1)
    '("SSubSubSubSection" 1)
    '("Ssubsubsubsubsectiongrouperstarx" 2)
    '("Ssubsubsubsectiongrouperstarx" 2)
    '("Ssubsubsectiongrouperstarx" 2)
    '("Ssubsectiongrouperstarx" 2)
    '("SNumberOfAuthors" 1)
    '("SVersionBefore" 1)
    '("SAuthorSep" 1)
    '("SAuthor" 1)
    '("titleAndEmptyVersionAndEmptyAuthorsAndShort" 4)
    '("titleAndEmptyVersionAndAuthorsAndShort" 4)
    '("titleAndVersionAndEmptyAuthorsAndShort" 4)
    '("titleAndVersionAndAuthorsAndShort" 4)
    '("titleAndEmptyVersionAndEmptyAuthors" 3)
    '("titleAndEmptyVersionAndAuthors" 3)
    '("titleAndVersionAndEmptyAuthors" 3)
    '("titleAndVersionAndAuthors" 3)
    '("refelemleft" 1)
    '("refparaleft" 1)
    '("refelem" 1)
    '("refpara" 1)
    '("compactItem" 1)
    '("SVInsetBox" 1)
    '("SCodeInsetBox" 1)
    '("Sendsentence" 1)
    '("Sendabbrev" 1)
    '("atItemizeStart" 0)
    '("bigtableinlinecorrect" 0)
    '("SEndFirstHead" 0)
    '("slant" 1)
    '("planetName" 1)
    '("Larger" 1)
    '("Smaller" 1)
    '("noborder" 1)
    '("indexlink" 1)
    '("badlink" 1)
    '("techinside" 1)
    '("techoutside" 1)
    '("plainlink" 1)
    '("inrgbcolorbox" 2)
    '("incolorbox" 2)
    '("intextrgbcolor" 2)
    '("intextcolor" 2)
    '("textsuper" 1)
    '("textsub" 1)
    '("Scribtexttt" 1)
    '("Smanypageref" 1)
    '("SectionNumberLink" 2)
    '("PartRefLocalUCUN" 2)
    '("SecRefLocalUCUN" 2)
    '("ChapRefLocalUCUN" 2)
    '("BookRefLocalUCUN" 2)
    '("PartRefLocalUN" 2)
    '("SecRefLocalUN" 2)
    '("ChapRefLocalUN" 2)
    '("BookRefLocalUN" 2)
    '("PartRefUCUN" 1)
    '("SecRefUCUN" 1)
    '("ChapRefUCUN" 1)
    '("BookRefUCUN" 1)
    '("PartRefUN" 1)
    '("SecRefUN" 1)
    '("ChapRefUN" 1)
    '("BookRefUN" 1)
    '("PartRefLocalUC" 3)
    '("SecRefLocalUC" 3)
    '("ChapRefLocalUC" 3)
    '("BookRefLocalUC" 3)
    '("PartRefLocal" 3)
    '("SecRefLocal" 3)
    '("ChapRefLocal" 3)
    '("BookRefLocal" 3)
    '("PartRefUC" 2)
    '("SecRefUC" 2)
    '("BookRefUC" 2)
    '("PartRef" 2)
    '("SecRef" 2)
    '("BookRef" 2)
    "packageGraphicx"
    "packageHyperref"
    "renewrmdefault"
    "packageRelsize"
    "packageAmsmath"
    "packageMathabx"
    "packageWasysym"
    "packageTxfonts"
    "packageTextcomp"
    "packageFramed"
    "packageHyphenat"
    "packageColor"
    "doHypersetup"
    "packageTocstyle"
    "packageCJK"
    "sectionNewpage"
    "partNewpage"
    "preDoc"
    "postDoc"
    "Stttextmore"
    "Stttextless"
    "Stttextbar"
    "SBoxedLeft"
    "bigtableleftpad"
    "notitlesection"
    "Sincpart"
    "Sincsection"
    "Sincsubsection"
    "Sincsubsubsection"
    "Sincsubsubsubsection"
    "Sincsubsubsubsubsection"
    "SOpenSq"
    "SCloseSq"
    "leftmoon"
    "rightmoon"
    "fullmoon"
    "newmoon"
    "diameter"
    "widering"
    "protect"
    "emptyrow"
    "tabrow"
    "tabularnewline"
    "SOriginalthesubsection"
    "SOriginalthesubsubsection"
    "thesubsection"
    "thesubsubsection"
    "math"
    "texMathInline"
    "texMathDisplay"
    "upint"
    "lowint")
   (LaTeX-add-labels
    "t:x28part_x22Modelsx5fofx5fSpikingx5fNeuronsx5fandx5fDifferentialx5fEquationsx22x29"
    "t:x28part_x22Anx5fIntroductionx5ftox5fDifferentialx5fEquationsx22x29"
    "t:x28part_x22Multiplex5fWaysx5ftox5fSayx5fthex5fSamex5fThingx22x29"
    "t:x28part_x22Derivativesx5farex5fSlopesx22x29"
    "definition:derivative")
   (LaTeX-add-environments
    "pltstabular"
    "bigtabular"
    "SingleColumn"
    "Subflow"
    "SInsetFlow"
    "SCodeFlow"
    "SVInsetFlow"
    "compact"
    "SCentered"
    "refcolumn"
    "refcontent"
    "refcolumnleft"
    "SVerbatim"
    "RktBlk"
    "defmodule"
    "prototype"
    "argcontract"
    "together"
    "specgrammar"
    "RBibliography"
    "leftindent"
    "insetpara"
    "Rfilecontent"
    "RBackgroundLabelInner")
   (LaTeX-add-counters
    "GrouperTemp")
   (LaTeX-add-lengths
    "stabLeft")
   (LaTeX-add-color-definecolors
    "PaleBlue"
    "LightGray"
    "CommentColor"
    "ParenColor"
    "IdentifierColor"
    "ResultColor"
    "ValueColor"
    "OutputColor")
   (LaTeX-add-amsthm-newtheorems
    "definition"
    "theorem"
    "conjecture"
    "lemma"
    "property"
    "notation"))
 :latex)

