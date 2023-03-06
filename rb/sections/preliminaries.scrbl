#lang scribble/book

@(require plot/pict
          scribble/manual
	  scribble/base
	  scribble-math/dollar
	  scribble/example
	  racket/math
          scriblib/figure)

@title{Preliminaries}

@section{Racket}

For this course we will be writing our code in @hyperlink["https://racket-lang.org/"]{Racket}. Racket is in the category of @emph{LISP}s, and is a descendant of @emph{Scheme}. These languages are in the tradition of good old fashioned AI (GOFAI). Their heritage is in symbolic computation, and connects programming with formal models of computation such as the λ calculus. Knowing an example of this language family is good for developing programming knowledge and helping to see the big picture. Programming is more than a particular languge or syntax, it is a medium for expressing ideas. Learning more than one way to express oneself programmatically helps to abstract the message out of the medium. Racket offers the practical advantage that it comes with good support for all popular operating systems and most hardware. Racket incorporates a @hyperlink["https://docs.racket-lang.org/quick/index.html"]{picture aware} integrated development environment with syntax highlighting and has long been a staple of programming education with extensive tutorial material. At the same time Racket is a modern programming language in which one can write "production" code (though admitedly few do). It is touted as a @hyperlink["https://cacm.acm.org/magazines/2018/3/225475-a-programmable-programming-language/fulltext"]{programming language for writing programming languages}, thus a student fully familiar with Racket can do more than most when it comes to coding. Though it is not the language of choice if one wants to get a job, it's selection here emphasizes our perspective on trying to understand the nature of the methods more than how to scale them or optimize run-time efficiencies.

@subsection{Getting Racket}

@itemlist[#:style 'ordered @item{Go to @hyperlink["https://www.racket-lang.org"]{racket-lang.org} and download and install the proper version for you operating system}
               @item{Verify you can open Dr. Racket}
               @item{Verify that Dr. Racket works by entering a simple instruction in the top window and seeing it executed in the bottom window}]

@figure[
  "fig:drracket"
  @elem{The Dr Racket IDE with a @tt{#lang} line and some simple code}
  @elem{@image[#:scale 0.25]{./images/drrack.svg}}]

@bold{Submit} your screen shot of your Dr Racket IDE with your name to the appropriate Dropbox on Learn.

@section{Git}

This book and the code it uses is in a git repository. Currently the git repository for this book is hosted on @hyperlink["https:github.com/brittAnderson/compNeuroIntro420"]{github}. Make sure you have the @tt{racket-book} branch selected. You are free to look at other branches, and you may find some interesting code or examples there from earlier offerings of the course, but the branch that we will be using for the Fall 2022 term is the @tt{racket-book} branch.

@hyperlink["https://git-scm.com/"]{Git} is a  program for version control, and is very useful. @hyperlink["https://github.com"]{Github} is one of a few different hosting hubs where many developers host their code to make it visible to others. You can @tt{fork} and @tt{clone} the code of others to try out their software or make your own changes to it.

I tried to explain all this once in a video. If you are already very confused it will not make things worse, and if you are only mildly confused it might help.


@(require scribble-embedding)

@(youtube "https://player.vimeo.com/video/456349595?h=4b78ed9d6")



@subsection{Getting Git}

To make sure you have, and will be able to update, the code and material for this course you will need to clone this repository. If you wish to be able to make contributions to this course via a @emph{pull request} you will first need to fork this repo. To that you should,

@itemlist[#:style 'ordered @item{Get a @hyperlink["https://git-scm.com/downloads"]{program} for using git on your operating system installed,}
          @item{Clone (and possibly fork) this repository}
          @item{Demonstrate that you are tracking the correct branch.}]

One way to do this is to run the @tt{git branch --verbose} and @tt{git remote --verbose} commands from the terminal, take a screenshot (on my linux system I use the terminal screen shot library @hyperlink["https://wiki.archlinux.org/title/Screen_capture"]{scrot}).

@bold{Submit} the screenshot of the output to the Dropbox on Learn.

@section{Scribble}

@hyperlink["https://docs.racket-lang.org/scribble/index.html"]{Scribble} is the Racket documentation tool. The course material is being written in it. It is quite powerful right out of the box without too much tweaking, but to get more advanced features functioning well you may have to do some searching and import a number of additional racket packages. It is a version of a markup language (@emph{markdown} is another common markup language and @emph{jupyter} notebooks are yet another version of the same idea).

The advantage of these tools is that they allow you to blend both code and text in a single document. You can explain what you are doing as text with links and images like you would in a document. You can also include code like you would in a program. The code can be both written out like a quotation or actually run with the results of the code actually input into a document. This allows the possibility of @emph{literate programming} or @emph{reproducible research reports} or what we are doing here: treating the code as another type of data (like our words and references and images and videos) that we use to express our ideas as clearly as we can. I find the @emph{@bold{@tt{babel system of org-mode}}} to be the best of the bunch, but unfortunately it does not support Racket well. 

Some of the course assignments will require you to submit a scribble document. You can start out writing a simple scribble document in the Dr Racket IDE and using the examples on the Racket language website. I have also included in this repository a @hyperlink["./template-explained.scrbl"]{template scribble file} that includes a number of imports and has also been exported to html so that you can better understand what the scribble commands look like and what they are supposed to look like when you compile them (@hyperlink["./template-explained.html"]{template file explained and compiled}).




                     
