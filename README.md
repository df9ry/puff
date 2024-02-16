<h1> Free and open Microstrip Impedance Matching Development Tool PUFF </h1>

This is, as far as I know, the only usable design and simulation tool for developing microstrip matching networks that is free and open and therefore usable for the common microwave enthusiast and ham radio developer. Puff have a pretty long history. The first version where developed for DOS computers by some fellows at the California Institute of Technology. My dear friends Terry and Franz distributed and documented it well in the microwave magazine UKW-Berichte. At this time it could be purchased for 10$ from this company in Bayersdorf. Unfortunatelly they died in a crash with their Piper Seneca on the way home after work. I had the luck not to be on board for my english was so bad that I had to attend to a reexamination at the Fachhochschule Weiden OPf.. The company was sold and Puff vanished from the market.

Fortunatelly the Pascal sourcecode were delivered along with the DOS Program on the diskettes so that it later on can be released under an open source license. Then it was the great work of PA3FWM to porte the program to Linux with a X11 Interface (see: <[PA3FWM's GitHub project](https://www.pa3fwm.nl/software/puff/)> so that it can be used again. Then the used Pascal Language Dialect falls into oblivion and can not longer compiled on recent Linux Systems. To make Puff available again in recent Linux distributions I decidet to port the program to Ada.

<h2>Great thanks</h2>

Great thanks to all who contibuted to this software so far (Alphabetically order):

- A. Gerstlauer
- D.B. Rutledge.
- Michael Van Canneyt (Free Pascal Development Team)
- Peter Vreman (Free Pasca Development Team)
- Pieter-Tjerk de Boer, pa3fwm@amsat.org
- R.C. Compton
- S.W. Wedge
- Terry Bittan (UKW-Berichte)

<h2>Continued Work</h2>
I took the Pascal source code and translated it into Ada. To do so, I used the very helpful tools from the <[Pascal to Ada Converter Homepage](https://p2ada.sourceforge.net/)> and <[Pascal tzo Ada Converter GitHub Project](https://github.com/zertovitch/pascal-to-ada)>. The code produced from this converter lacks any useful indentation or formatting (1000s of error messages from GNAT), so I wrote a tool to scan the generated source and put it in a propper indentation and formatting (See: <[Ada Beautify Tool](https://github.com/df9ry/ada_beautify)>. Using Alire (See: <[Alire](https://alire.ada.dev/)>) I created a GNAT Studio Project (See: <[GNAT Studio](https://www.adacore.com/gnatpro/toolsuite/gnatstudio)>) and port routine by routine over to Ada. This is the work I am performing in the moment.

<h2>Roadmap</h2>

1. Translate Pascal Source Code to Ada Source Code using a translation Tool. **Status: Done**.
2. Port the C Part over to the new Project and update to recent API. **Status: Done**.
3. Show empty Window when starting the Program. **Status: Done**
4. Port routine by routine over to the new Project. **Status: In Progress**.
5. Complete the program so that it resembles to old DOS version complete. **Status: To Do**
6. Package the program into a regular Debian Archive and offer it to the Debian Software Archive. **Status: To Do**
7. Replace the old DOS - Style UI with modern GTK based interface. **Status: To Do**
8. Package it again into a regular Debian Archive and offer it as Puff V2 again. **Status: To Do**

<h2>Contact information</h2>

If you want to know more ore have some ideas, feel free to contact me on <df9ry@darc.de>. You are most welcome.

---

END OF DOCUMENT


 

