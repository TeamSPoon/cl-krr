			       KMgen installation
			           12 July 2009
			  Francis Leboutte - www.algo.be
			  ------------------------------

- Installation
- Documentation
- Test, demo
- Mailing list

Installation
============

KMgen requirements
------------------

- Operating System: Windows 2000 or XP (not tested on Vista)

- Java
Java Runtime Environment (JRE)
http://java.com/

- GNU Emacs
http://www.gnu.org/software/emacs/emacs.html

- PostgreSQL
An account for a PostgreSQL database server is needed.
The server can be anywhere on your PC, your local area network or the Internet.
http://www.postgresql.org/

PostgreSQL installation
-----------------------
At installation time:
  1. Leave the port number unchanged.
  2. Select the C locale (locale to be used by the new database cluster).
     If the locale is C (or equivalently POSIX), then all character set encodings
     are allowed for new databases to be created. KMgen uses Latin-1 DB encoding.
  Note:
  On Linux, to delete and recreate the main PostgreSQL server cluster with LATIN1
  being the default encoding of any database to create later:
  - pg_dropcluster 8.4 main --stop
  - pg_createcluster 8.4 main --start -e "LATIN1"

If you want to use the copy, dump and restore KMgen commands,
the following PostgreSQL utilities must be on the Windows path
of the PC where KMgen is installed:
  - pg_dump.exe
  - createdb.exe
  - pg_restore.exe

KMgen Installation
------------------
Copy the KMgen distribution files on the hard disk.
Make a shortcut to the KMgen.exe or KMgen.bat file (see below).

GNU Emacs installation
----------------------
After unpacking the Emacs archive file, run addpm.exe which is in the bin
directory of the Emacs distribution. This is mandatory for KMgen.

If you haven't already a HOME environment variable you should define it:
- follow Start > Settings > Control Panel > System. Then select Advanced followed
  by Environment Variables. Then, add HOME as a user variable with the value being
  the path of a particular directory (for example, d:\home).
- Logout and login.

From the \home directory in the KMgen distribution files:
- copy the _emacs file in you home directory (if you already have an _emacs or .emacs
  file, include the KMgen startup file in your file).
- and copy the \KMgen\ directory into your home directory.

In KMgen, choose between gnuclient and EmacsClient (gnuclient and EmacsClient allow
KMgen to edit KM forms in Emacs).
If you use GnuEmacs below 22.1, KMgen has to use gnuclientw.exe. Select
the Tools menu, then Options and Emacs, and replace the default values
in this way:
   - Emacs client: gnuclientw.exe
   - wait switches: -sfw
   - no wait switches: -sqf
Default values are for GnuEmacs 22.1 and above:
    - Emacs client: emacsclientw.exe
    - wait switches:        (empty)
    - no wait switches:     (empty)

With GnuEmacs 22.1 and above you have to start Emacs before to use it from KMgen.
You could do this in tree ways:
1. Start Emacs manually before KMgen.
2. Start Emacs and KMgen using the \bin\KMgen.bat procedure (don't forget to
   update the path to emacs in this file). The preferred way.
3. Another possibility would be to use the patched version of EmacsW32 where Emacs and
   emacs server will be started automatically when emacsclient is started from KMgen.
   Look at http://ourcomments.org/Emacs/EmacsW32Util.html
   In this case remove the line (server-start) in you Emacs initialization file.

Documentation
=============
See KMgen-guide.pdf for some hints.

Test, demo
==========
To run KMgen double click on the KMgen shortcut.
In the Ontology menu, choose:
- "Choose server" to define the default PostgreSQL server.
  In this dialog enter the server name or IP address and a PG (PostgreSQL) user
  account name and password.
- "New" to create a new ontology (database).
- "KMgen user" to change the default KMgen username from "system" to what you want
- "Import from KM file" and select one of the KM files in the demo directory:
  1. Clib.km (Component library - Knowledge Systems Group, UT-Austin).
  2. go.km file (can take a while - about 8000 frames).

Troubleshooting
---------------
If KMgen does not start properly, check the processes list in the Task manager and
kill remaining cmd, java and KMgen processes. Start KMgen again.

PostgreSQL server connexion problem: look in the KMgen-guid.pdf file.

Mailing list
============
 For announcements, bug reports and questions.
See http://algo.be/mailman/listinfo/kmgen_algo.be
