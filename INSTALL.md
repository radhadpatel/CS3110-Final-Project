# CS 3110 Final Project 

**Members:** Chris O'Brian (co253), Lorenzo Scotto di Vettimo (ls769), Radha Patel (rdp89)


## Installation Instructions
---
Our system requires the "Ocurl" package to function, to install, please run:

```
opam install ocurl
```

After which, you should run "make check" to ensure that your environment is what
it should be. We've edited the checkenv.sh file to reflect our system.

Then, you can start our program by running "make run".

**NOTE:** You can always just press Enter/Return at the main prompt for info
on what possible commands can be used. Then, entering only that command with no
arguments will either execute the command (like "clear" or "quit"), or it will
provide info on what other arguments are required.

## Some example commands for MS1 functionality:

Follow the instructions at the initial prompt to either create a new schedule
or load an existing one. We have provided an example.json in the working 
directory to load as an example. Try entering ONE of the two possible commands 
below:

```
new TEST ENG
load example.json
```

Once a schedule is open, you can use the "print" command at any time to view
the semesters/courses in the schedule:
```
print
```


To create a new semester(s):
```
add FA19
add SP20
```

Add a new course:
(Here, 4 is # of credits, A- is grade, "Core" is degree catagory, FA19 is
 semester).
```
add CS3110 4 A- Core FA19 
```

Add a new course (and have Class Roster get credits info):
```
add CS3410 B Core FA19 
```

Edit course attribute (like credits):
_Notice how GPA changes with this (by running print again)_
```
edit CS3110 credits 3
```

Remove a course:
```
remove CS3110
```

Remove a semester:
```
remove SP20
```

To exit:
```
quit
```

## Some example commands for MS2 functionality:

MS2 contains all of the same functionality as MS1, but the user now has the
ability to save a current schedule, load that schedule later, "close" the
current schedule, and export a schedule to an HTML file for visualization.

Saving a schedule:
_This command saves schedule as test.json in current working directory._
```
save test.json
```

Exporting a schedule to HTML:
_Exports HTML file to working directory._
```
export test.html
```

You can now open the html file in a web browser to see a nice visualization of
the schedule!

You can also close a current schedule to return to the initial prompt:
_Note: If you haven't saved your schedule it will prompt you to do so!_
```
close
```

A similar "save? prompt" will appear when you try to quit now!



## Example commands for MS3 functionality:

We've now added the ability for a user to add courses from a semester built 
with the Scheduler tool on Cornell's Class Roster website. This entails building
the semester online, then downloading the iCal (.ics) file. This sem can be 
imported to a schedule using:

```
import example.ics
```

We've included an "example.ics" to try this command wth.

_Notice_ that this command will create a new semester if the semester doesn't
already exist automatically. It will not overwrite any already existing courses
in a given semester.


We've also added the _swap_ and _move_ commands to swap two courses from 
semester to semester, and to just move a course to another semester:

Assuming you've just loaded example.json, you can use the following command
to move PHYS2213 to the FA19 semester:
```
move PHYS2213 FA19
```

and the following command to swap the semester-position of CS4820 with CS3110:
```
swap CS4820 CS3110
```

Finally, the best part -- you can now check a schedule against a set of 
hardcoded requirements! This can be done using the check command (below). The
system will print any missing courses or credits for you.
```
check
```

Now, we've only hardcoded the CS requirements for both ENG and A&S students. 
You can set which "school" you're in by entering one of the two commands:
```
edit school ENG
edit school CAS
```

This validation is also now included in all exported HTML files.


We've also added a user preferences feature! You can now edit the HTML colors
in the export file, as well as toggle an autosave feature. You can use the 
"set" command to do so:
```
set autosave true
set html_background_color blue
set html_square_color red
```

Finally, we've also included the ability to view course details by running
"print" followed by a course name:
```
print CS2800
```