(setq company-netlogo-functions `(
( "abs" ,(concat "abs " (propertize "number " 'face 'italic) ) . 
"Reports the absolute value of number" ) 
( "acos" ,(concat "acos " (propertize "number " 'face 'italic) ) . 
"Reports the arc cosine (inverse cosine) of the given number" ) 
( "all?" ,(concat "all? " (propertize "agentset " 'face 'italic) (propertize "[reporter] " 'face 'italic) ) . 
"Reports true if all of the agents in the agentset report true for the given reporter" ) 
( "and" ,(concat (propertize "condition1 " 'face 'italic) "and " (propertize "condition2 " 'face 'italic) ) . 
"Reports true if both condition1 and condition2 are true" ) 
( "any?" ,(concat "any? " (propertize "agentset " 'face 'italic) ) . 
"Reports true if the given agentset is non-empty, false otherwise" ) 
( "approximate-hsb" ,(concat "approximate-hsb " (propertize "hue " 'face 'italic) (propertize "saturation " 'face 'italic) (propertize "brightness " 'face 'italic) ) . 
"Reports a number in the range 0 to 140, not including 140 itself, that represents the given color, specified in the HSB spectrum, in NetLogo's color space" ) 
( "approximate-rgb" ,(concat "approximate-rgb " (propertize "red " 'face 'italic) (propertize "green " 'face 'italic) (propertize "blue " 'face 'italic) ) . 
"Reports a number in the range 0 to 140, not including 140 itself, that represents the given color, specified in the RGB spectrum, in NetLogo's color space" ) 
( "asin" ,(concat "asin " (propertize "number " 'face 'italic) ) . 
"Reports the arc sine (inverse sine) of the given number" ) 
( "ask" ,(concat "ask " (propertize "agentset " 'face 'italic) (propertize "[commands] " 'face 'italic) ) . 
"The specified agent or agentset runs the given commands" ) 
( "ask" ,(concat "ask " (propertize "agent " 'face 'italic) (propertize "[commands] " 'face 'italic) ) . 
"The specified agent or agentset runs the given commands" ) 
( "ask-concurrent" ,(concat "ask-concurrent " (propertize "agentset " 'face 'italic) (propertize "[commands] " 'face 'italic) ) . 
"This primitive exists only for backwards compatibility" ) 
( "at-points" ,(concat (propertize "agentset " 'face 'italic) "at-points " (propertize "[[x1 " 'face 'italic) (propertize "y1] " 'face 'italic) (propertize "[x2 " 'face 'italic) (propertize "y2] " 'face 'italic) (propertize "...] " 'face 'italic) ) . 
"Reports a subset of the given agentset that includes only the agents on the patches the given distances away from this agent" ) 
( "atan" ,(concat "atan " (propertize "x " 'face 'italic) (propertize "y " 'face 'italic) ) . 
"Converts x and y offsets to a turtle heading in degrees (from 0 to 360)" ) 
( "autoplot?" ,(concat "autoplot? " ) . 
"Reports true if auto-plotting is on for the current plot, false otherwise" ) 
( "auto-plot-off" ,(concat "auto-plot-off " ) . 
"This pair of commands is used to control the NetLogo feature of auto-plotting in the current plot" ) 
( "auto-plot-on" ,(concat "auto-plot-on " ) . 
"This pair of commands is used to control the NetLogo feature of auto-plotting in the current plot" ) 
( "back" ,(concat "back " (propertize "number " 'face 'italic) ) . 
"The turtle moves backward by number steps" ) 
( "base-colors" ,(concat "base-colors " ) . 
"Reports a list of the 14 basic NetLogo hues" ) 
( "beep" ,(concat "beep " ) . 
"Emits a beep" ) 
( "behaviorspace-experiment-name" ,(concat "behaviorspace-experiment-name " ) . 
"Reports the current experiment name in the current experiment" ) 
( "behaviorspace-run-number" ,(concat "behaviorspace-run-number " ) . 
"Reports the current run number in the current BehaviorSpace experiment, starting at 1" ) 
( "both-ends" ,(concat "both-ends " ) . 
"Reports the agentset of the 2 nodes connected by this link" ) 
( "breed" ,(concat "breed " ) . 
"This is a built-in turtle and link variable" ) 
( "breed" ,(concat "breed " (propertize "[<breeds> " 'face 'italic) (propertize "<breed>] " 'face 'italic) ) . 
"This keyword, like the globals, turtles-own, and patches-own keywords, can only be used at the beginning of the Code tab, before any procedure definitions" ) 
( "but-first" ,(concat "but-first " (propertize "list " 'face 'italic) ) . 
"When used on a list, but-first reports all of the list items of list except the first, and but-last reports all of the list items of list except the last" ) 
( "but-first" ,(concat "but-first " (propertize "string " 'face 'italic) ) . 
"When used on a list, but-first reports all of the list items of list except the first, and but-last reports all of the list items of list except the last" ) 
( "but-last" ,(concat "but-last " (propertize "list " 'face 'italic) ) . 
"When used on a list, but-first reports all of the list items of list except the first, and but-last reports all of the list items of list except the last" ) 
( "but-last" ,(concat "but-last " (propertize "string " 'face 'italic) ) . 
"When used on a list, but-first reports all of the list items of list except the first, and but-last reports all of the list items of list except the last" ) 
( "can-move?" ,(concat "can-move? " (propertize "distance " 'face 'italic) ) . 
"Reports true if this turtle can move distance in the direction it is facing without violating the topology; reports false otherwise" ) 
( "carefully" ,(concat "carefully " (propertize "[ " 'face 'italic) (propertize "commands1 " 'face 'italic) (propertize "] " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands2 " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Runs commands1" ) 
( "ceiling" ,(concat "ceiling " (propertize "number " 'face 'italic) ) . 
"Reports the smallest integer greater than or equal to number" ) 
( "clear-all" ,(concat "clear-all " ) . 
"Combines the effects of clear-globals, clear-ticks, clear-turtles, clear-patches, clear-drawing, clear-all-plots, and clear-output" ) 
( "clear-all-plots" ,(concat "clear-all-plots " ) . 
"Clears every plot in the model" ) 
( "clear-drawing" ,(concat "clear-drawing " ) . 
"Clears all lines and stamps drawn by turtles" ) 
( "clear-globals" ,(concat "clear-globals " ) . 
"Sets all global variables to 0" ) 
( "clear-links" ,(concat "clear-links " ) . 
"Kills all links" ) 
( "clear-output" ,(concat "clear-output " ) . 
"Clears all text from the model's output area, if it has one" ) 
( "clear-patches" ,(concat "clear-patches " ) . 
"Clears the patches by resetting all patch variables to their default initial values, including setting their color to black" ) 
( "clear-plot" ,(concat "clear-plot " ) . 
"In the current plot only, resets all plot pens, deletes all temporary plot pens, resets the plot to its default values (for x range, y range, etc" ) 
( "clear-ticks" ,(concat "clear-ticks " ) . 
"Clears the tick counter" ) 
( "clear-turtles" ,(concat "clear-turtles " ) . 
"Kills all turtles" ) 
( "color" ,(concat "color " ) . 
"This is a built-in turtle or link variable" ) 
( "cos" ,(concat "cos " (propertize "number " 'face 'italic) ) . 
"Reports the cosine of the given angle" ) 
( "count" ,(concat "count " (propertize "agentset " 'face 'italic) ) . 
"Reports the number of agents in the given agentset" ) 
( "create-ordered-turtles" ,(concat "create-ordered-turtles " (propertize "number " 'face 'italic) ) . 
"Creates number new turtles" ) 
( "create-ordered-turtles" ,(concat "create-ordered-turtles " (propertize "number " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Creates number new turtles" ) 
( "create-ordered" ,(concat (propertize "create-ordered<breeds> " 'face 'italic) (propertize "number " 'face 'italic) ) . 
"Creates number new turtles" ) 
( "create-ordered" ,(concat (propertize "create-ordered<breeds> " 'face 'italic) (propertize "number " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Creates number new turtles" ) 
( "create-<breed>-to" ,(concat "create-<breed>-to " (propertize "turtle " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breed>-to" ,(concat "create-<breed>-to " (propertize "turtle " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breed>-from" ,(concat "create-<breed>-from " (propertize "turtle " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breed>-from" ,(concat "create-<breed>-from " (propertize "turtle " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breed>-with" ,(concat "create-<breed>-with " (propertize "turtle " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breed>-with" ,(concat "create-<breed>-with " (propertize "turtle " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breeds>-to" ,(concat "create-<breeds>-to " (propertize "turtleset " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breeds>-to" ,(concat "create-<breeds>-to " (propertize "turtleset " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breeds>-from" ,(concat "create-<breeds>-from " (propertize "turtleset " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breeds>-from" ,(concat "create-<breeds>-from " (propertize "turtleset " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breeds>-with" ,(concat "create-<breeds>-with " (propertize "turtleset " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-<breeds>-with" ,(concat "create-<breeds>-with " (propertize "turtleset " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-link-to" ,(concat "create-link-to " (propertize "turtle " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-link-to" ,(concat "create-link-to " (propertize "turtle " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-link-from" ,(concat "create-link-from " (propertize "turtle " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-link-from" ,(concat "create-link-from " (propertize "turtle " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-link-with" ,(concat "create-link-with " (propertize "turtle " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-link-with" ,(concat "create-link-with " (propertize "turtle " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-links-to" ,(concat "create-links-to " (propertize "turtleset " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-links-to" ,(concat "create-links-to " (propertize "turtleset " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-links-from" ,(concat "create-links-from " (propertize "turtleset " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-links-from" ,(concat "create-links-from " (propertize "turtleset " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-links-with" ,(concat "create-links-with " (propertize "turtleset " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-links-with" ,(concat "create-links-with " (propertize "turtleset " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Used for creating breeded and unbreeded links between turtles" ) 
( "create-turtles" ,(concat "create-turtles " (propertize "number " 'face 'italic) ) . 
"Creates number new turtles at the origin" ) 
( "create-turtles" ,(concat "create-turtles " (propertize "number " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Creates number new turtles at the origin" ) 
( "create-" ,(concat (propertize "create-<breeds> " 'face 'italic) (propertize "number " 'face 'italic) ) . 
"Creates number new turtles at the origin" ) 
( "create-" ,(concat (propertize "create-<breeds> " 'face 'italic) (propertize "number " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Creates number new turtles at the origin" ) 
( "create-temporary-plot-pen" ,(concat "create-temporary-plot-pen " (propertize "string " 'face 'italic) ) . 
"A new temporary plot pen with the given name is created in the current plot and set to be the current pen" ) 
( "date-and-time" ,(concat "date-and-time " ) . 
"Reports a string containing the current date and time" ) 
( "die" ,(concat "die " ) . 
"The turtle or link dies" ) 
( "diffuse" ,(concat "diffuse " (propertize "patch-variable " 'face 'italic) (propertize "number " 'face 'italic) ) . 
"Tells each patch to give equal shares of (number * 100) percent of the value of patch-variable to its eight neighboring patches" ) 
( "diffuse4" ,(concat "diffuse4 " (propertize "patch-variable " 'face 'italic) (propertize "number " 'face 'italic) ) . 
"Like diffuse, but only diffuses to the four neighboring patches (to the north, south, east, and west), not to the diagonal neighbors" ) 
( "directed-link-breed" ,(concat "directed-link-breed " (propertize "[<link-breeds> " 'face 'italic) (propertize "<link-breed>] " 'face 'italic) ) . 
"This keyword, like the globals and breeds keywords, can only be used at the beginning of the Code tab, before any procedure definitions" ) 
( "display" ,(concat "display " ) . 
"Causes the view to be updated immediately" ) 
( "distance" ,(concat "distance " (propertize "agent " 'face 'italic) ) . 
"Reports the distance from this agent to the given turtle or patch" ) 
( "distancexy" ,(concat "distancexy " (propertize "xcor " 'face 'italic) (propertize "ycor " 'face 'italic) ) . 
"Reports the distance from this agent to the point (xcor, ycor)" ) 
( "downhill" ,(concat "downhill " (propertize "patch-variable " 'face 'italic) ) . 
"Moves the turtle to the neighboring patch with the lowest value for patch-variable" ) 
( "downhill4" ,(concat "downhill4 " (propertize "patch-variable " 'face 'italic) ) . 
"Moves the turtle to the neighboring patch with the lowest value for patch-variable" ) 
( "dx" ,(concat "dx " ) . 
"Reports the x-increment or y-increment (the amount by which the turtle's xcor or ycor would change) if the turtle were to take one step forward in its current heading" ) 
( "dy" ,(concat "dy " ) . 
"Reports the x-increment or y-increment (the amount by which the turtle's xcor or ycor would change) if the turtle were to take one step forward in its current heading" ) 
( "empty?" ,(concat "empty? " (propertize "list " 'face 'italic) ) . 
"Reports true if the given list or string is empty, false otherwise" ) 
( "empty?" ,(concat "empty? " (propertize "string " 'face 'italic) ) . 
"Reports true if the given list or string is empty, false otherwise" ) 
( "end" ,(concat "end " ) . 
"Used to conclude a procedure" ) 
( "end1" ,(concat "end1 " ) . 
"This is a built-in link variable" ) 
( "end2" ,(concat "end2 " ) . 
"This is a built-in link variable" ) 
( "error" ,(concat "error " (propertize "value " 'face 'italic) ) . 
"Causes a runtime error to occur" ) 
( "error-message" ,(concat "error-message " ) . 
"Reports a string describing the error that was suppressed by carefully" ) 
( "every" ,(concat "every " (propertize "number " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Runs the given commands only if it's been more than number seconds since the last time this agent ran them in this context" ) 
( "exp" ,(concat "exp " (propertize "number " 'face 'italic) ) . 
"Reports the value of e raised to the number power" ) 
( "export-view" ,(concat "export-view " (propertize "filename " 'face 'italic) ) . 
"export-view writes the current contents of the current view to an external file given by the string filename" ) 
( "export-interface" ,(concat "export-interface " (propertize "filename " 'face 'italic) ) . 
"export-view writes the current contents of the current view to an external file given by the string filename" ) 
( "export-output" ,(concat "export-output " (propertize "filename " 'face 'italic) ) . 
"export-view writes the current contents of the current view to an external file given by the string filename" ) 
( "export-plot" ,(concat "export-plot " (propertize "plotname " 'face 'italic) (propertize "filename " 'face 'italic) ) . 
"export-view writes the current contents of the current view to an external file given by the string filename" ) 
( "export-all-plots" ,(concat "export-all-plots " (propertize "filename " 'face 'italic) ) . 
"export-view writes the current contents of the current view to an external file given by the string filename" ) 
( "export-world" ,(concat "export-world " (propertize "filename " 'face 'italic) ) . 
"export-view writes the current contents of the current view to an external file given by the string filename" ) 
( "extensions" ,(concat "extensions " (propertize "[name " 'face 'italic) (propertize "...] " 'face 'italic) ) . 
"Allows the model to use primitives from the extensions with the given names" ) 
( "extract-hsb" ,(concat "extract-hsb " (propertize "color " 'face 'italic) ) . 
"Reports a list of three values, the first (hue) in the range of 0 to 360, the second and third (brightness and saturation) in the range of in the range 0 to 100, of the given NetLogo color in the range 0 to 140, not including 140 itself" ) 
( "extract-rgb" ,(concat "extract-rgb " (propertize "color " 'face 'italic) ) . 
"Reports a list of three values in the range 0 to 255 representing the levels of red, green, and blue, respectively, of the given NetLogo color in the range 0 to 140, not including 140 itself" ) 
( "face" ,(concat "face " (propertize "agent " 'face 'italic) ) . 
"Set the caller's heading towards agent" ) 
( "facexy" ,(concat "facexy " (propertize "number " 'face 'italic) (propertize "number " 'face 'italic) ) . 
"Set the caller's heading towards the point (x,y)" ) 
( "file-at-end?" ,(concat "file-at-end? " ) . 
"Reports true when there are no more characters left to read in from the current file (that was opened previously with file-open)" ) 
( "file-close" ,(concat "file-close " ) . 
"Closes a file that has been opened previously with file-open" ) 
( "file-close-all" ,(concat "file-close-all " ) . 
"Closes all files (if any) that have been opened previously with file-open" ) 
( "file-delete" ,(concat "file-delete " (propertize "string " 'face 'italic) ) . 
"Deletes the file specified as string" ) 
( "file-exists?" ,(concat "file-exists? " (propertize "string " 'face 'italic) ) . 
"Reports true if string is the name of an existing file on the system" ) 
( "file-flush" ,(concat "file-flush " ) . 
"Forces file updates to be written to disk" ) 
( "file-open" ,(concat "file-open " (propertize "string " 'face 'italic) ) . 
"This command will interpret string as a path name to a file and open the file" ) 
( "file-print" ,(concat "file-print " (propertize "value " 'face 'italic) ) . 
"Prints value to an opened file, followed by a carriage return" ) 
( "file-read" ,(concat "file-read " ) . 
"This reporter will read in the next constant from the opened file and interpret it as if it had been typed in the Command Center" ) 
( "file-read-characters" ,(concat "file-read-characters " (propertize "number " 'face 'italic) ) . 
"Reports the given number of characters from an opened file as a string" ) 
( "file-read-line" ,(concat "file-read-line " ) . 
"Reads the next line in the file and reports it as a string" ) 
( "file-show" ,(concat "file-show " (propertize "value " 'face 'italic) ) . 
"Prints value to an opened file, preceded by this agent agent, and followed by a carriage return" ) 
( "file-type" ,(concat "file-type " (propertize "value " 'face 'italic) ) . 
"Prints value to an opened file, not followed by a carriage return (unlike file-print and file-show)" ) 
( "file-write" ,(concat "file-write " (propertize "value " 'face 'italic) ) . 
"This command will output value, which can be a number, string, list, boolean, or nobody to an opened file, not followed by a carriage return (unlike file-print and file-show)" ) 
( "filter" ,(concat "filter " (propertize "reporter-task " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"Reports a list containing only those items of list for which the task reports true -- in other words, the items satisfying the given condition" ) 
( "first" ,(concat "first " (propertize "list " 'face 'italic) ) . 
"On a list, reports the first (0th) item in the list" ) 
( "first" ,(concat "first " (propertize "string " 'face 'italic) ) . 
"On a list, reports the first (0th) item in the list" ) 
( "floor" ,(concat "floor " (propertize "number " 'face 'italic) ) . 
"Reports the largest integer less than or equal to number" ) 
( "follow" ,(concat "follow " (propertize "turtle " 'face 'italic) ) . 
"Similar to ride, but, in the 3D view, the observer's vantage point is behind and above turtle" ) 
( "follow-me" ,(concat "follow-me " ) . 
"Asks the observer to follow this turtle" ) 
( "foreach" ,(concat "foreach " (propertize "list " 'face 'italic) (propertize "command-task " 'face 'italic) ) . 
"With a single list, runs the task for each item of list" ) 
( "(foreach" ,(concat "(foreach " (propertize "list1 " 'face 'italic) (propertize "... " 'face 'italic) (propertize "command-task) " 'face 'italic) ) . 
"With a single list, runs the task for each item of list" ) 
( "forward" ,(concat "forward " (propertize "number " 'face 'italic) ) . 
"The turtle moves forward by number steps, one step at a time" ) 
( "fput" ,(concat "fput " (propertize "item " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"Adds item to the beginning of a list and reports the new list" ) 
( "globals" ,(concat "globals " (propertize "[var1 " 'face 'italic) (propertize "...] " 'face 'italic) ) . 
"This keyword, like the breed, <breeds>-own, patches-own, and turtles-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "hatch" ,(concat "hatch " (propertize "number " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"This turtle creates number new turtles" ) 
( "hatch-" ,(concat (propertize "hatch-<breeds> " 'face 'italic) (propertize "number " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"This turtle creates number new turtles" ) 
( "heading" ,(concat "heading " ) . 
"This is a built-in turtle variable" ) 
( "hidden?" ,(concat "hidden? " ) . 
"This is a built-in turtle or link variable" ) 
( "hide-link" ,(concat "hide-link " ) . 
"The link makes itself invisible" ) 
( "hide-turtle" ,(concat "hide-turtle " ) . 
"The turtle makes itself invisible" ) 
( "histogram" ,(concat "histogram " (propertize "list " 'face 'italic) ) . 
"Histograms the values in the given list" ) 
( "home" ,(concat "home " ) . 
"This turtle moves to the origin (0,0)" ) 
( "hsb" ,(concat "hsb " (propertize "hue " 'face 'italic) (propertize "saturation " 'face 'italic) (propertize "brightness " 'face 'italic) ) . 
"Reports a RGB list when given three numbers describing an HSB color" ) 
( "hubnet-broadcast" ,(concat "hubnet-broadcast " (propertize "tag-name " 'face 'italic) (propertize "value " 'face 'italic) ) . 
"This broadcasts value from NetLogo to the interface element with the name tag-name on the clients" ) 
( "hubnet-broadcast-clear-output" ,(concat "hubnet-broadcast-clear-output " ) . 
"This clears all messages printed to the text area on every client" ) 
( "hubnet-broadcast-message" ,(concat "hubnet-broadcast-message " (propertize "value " 'face 'italic) ) . 
"This prints the value in the text area on each client" ) 
( "hubnet-clear-override" ,(concat "hubnet-clear-override " (propertize "client " 'face 'italic) (propertize "agent-or-set " 'face 'italic) (propertize "variable-name " 'face 'italic) ) . 
"Remove overrides from the override list on client" ) 
( "hubnet-clear-overrides" ,(concat "hubnet-clear-overrides " (propertize "client " 'face 'italic) ) . 
"Remove overrides from the override list on client" ) 
( "hubnet-clients-list" ,(concat "hubnet-clients-list " ) . 
"Reports a list containing the names of all the clients currently connected to the HubNet server" ) 
( "hubnet-enter-message?" ,(concat "hubnet-enter-message? " ) . 
"Reports true if a new client just entered the simulation" ) 
( "hubnet-exit-message?" ,(concat "hubnet-exit-message? " ) . 
"Reports true if a client just exited the simulation" ) 
( "hubnet-fetch-message" ,(concat "hubnet-fetch-message " ) . 
"If there is any new data sent by the clients, this retrieves the next piece of data, so that it can be accessed by hubnet-message, hubnet-message-source, and hubnet-message-tag" ) 
( "hubnet-kick-client" ,(concat "hubnet-kick-client " (propertize "client-name " 'face 'italic) ) . 
"Kicks the client with the given client-name" ) 
( "hubnet-kick-all-clients" ,(concat "hubnet-kick-all-clients " ) . 
"Kicks out all currently connected HubNet clients" ) 
( "hubnet-message" ,(concat "hubnet-message " ) . 
"Reports the message retrieved by hubnet-fetch-message" ) 
( "hubnet-message-source" ,(concat "hubnet-message-source " ) . 
"Reports the name of the client that sent the message retrieved by hubnet-fetch-message" ) 
( "hubnet-message-tag" ,(concat "hubnet-message-tag " ) . 
"Reports the tag that is associated with the data that was retrieved by hubnet-fetch-message" ) 
( "hubnet-message-waiting?" ,(concat "hubnet-message-waiting? " ) . 
"This looks for a new message sent by the clients" ) 
( "hubnet-reset" ,(concat "hubnet-reset " ) . 
"Starts up the HubNet system" ) 
( "hubnet-reset-perspective" ,(concat "hubnet-reset-perspective " (propertize "tag-name " 'face 'italic) ) . 
"Clears watch or follow sent directly to the client" ) 
( "hubnet-send" ,(concat "hubnet-send " (propertize "string " 'face 'italic) (propertize "tag-name " 'face 'italic) (propertize "value " 'face 'italic) ) . 
"For a string, this sends value from NetLogo to the tag tag-name on the client that has string for its user name" ) 
( "hubnet-send" ,(concat "hubnet-send " (propertize "list-of-strings " 'face 'italic) (propertize "tag-name " 'face 'italic) (propertize "value " 'face 'italic) ) . 
"For a string, this sends value from NetLogo to the tag tag-name on the client that has string for its user name" ) 
( "hubnet-send-clear-output" ,(concat "hubnet-send-clear-output " (propertize "string " 'face 'italic) ) . 
"This clears all messages printed to the text area on the given client or clients (specified in the string or list-of-strings" ) 
( "hubnet-send-clear-output" ,(concat "hubnet-send-clear-output " (propertize "list-of-strings " 'face 'italic) ) . 
"This clears all messages printed to the text area on the given client or clients (specified in the string or list-of-strings" ) 
( "hubnet-send-follow" ,(concat "hubnet-send-follow " (propertize "client-name " 'face 'italic) (propertize "agent " 'face 'italic) (propertize "radius " 'face 'italic) ) . 
"Tells the client associated with client-name to follow agent showing a radius sized Moore neighborhood around the agent" ) 
( "hubnet-send-message" ,(concat "hubnet-send-message " (propertize "string " 'face 'italic) (propertize "value " 'face 'italic) ) . 
"This prints value in the text area on the client specified by string" ) 
( "hubnet-send-override" ,(concat "hubnet-send-override " (propertize "client-name " 'face 'italic) (propertize "agent-or-set " 'face 'italic) (propertize "variable-name " 'face 'italic) ) . 
"Evaluates reporter for the agent or agentset indicated then sends the values to the client to 'override' the value of variable-name only on client-name" ) 
( "[" ,(concat "[ " (propertize "reporter " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Evaluates reporter for the agent or agentset indicated then sends the values to the client to 'override' the value of variable-name only on client-name" ) 
( "hubnet-send-watch" ,(concat "hubnet-send-watch " (propertize "client-name " 'face 'italic) (propertize "agent " 'face 'italic) ) . 
"Tells the client associated with client-name to watch agent" ) 
( "hubnet-set-client-interface" ,(concat "hubnet-set-client-interface " (propertize "client-type " 'face 'italic) (propertize "client-info " 'face 'italic) ) . 
"If client-type is 'COMPUTER', client-info is ignored" ) 
( "if" ,(concat "if " (propertize "condition " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Reporter must report a boolean (true or false) value" ) 
( "ifelse" ,(concat "ifelse " (propertize "reporter " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands1 " 'face 'italic) (propertize "] " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands2 " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Reporter must report a boolean (true or false) value" ) 
( "ifelse-value" ,(concat "ifelse-value " (propertize "reporter " 'face 'italic) (propertize "[reporter1] " 'face 'italic) (propertize "[reporter2] " 'face 'italic) ) . 
"Reporter must report a boolean (true or false) value" ) 
( "import-drawing" ,(concat "import-drawing " (propertize "filename " 'face 'italic) ) . 
"Reads an image file into the drawing, scaling it to the size of the world, while retaining the original aspect ratio of the image" ) 
( "import-pcolors" ,(concat "import-pcolors " (propertize "filename " 'face 'italic) ) . 
"Reads an image file, scales it to the same dimensions as the patch grid while maintaining the original aspect ratio of the image, and transfers the resulting pixel colors to the patches" ) 
( "import-pcolors-rgb" ,(concat "import-pcolors-rgb " (propertize "filename " 'face 'italic) ) . 
"Reads an image file, scales it to the same dimensions as the patch grid while maintaining the original aspect ratio of the image, and transfers the resulting pixel colors to the patches" ) 
( "import-world" ,(concat "import-world " (propertize "filename " 'face 'italic) ) . 
"Reads the values of all variables for a model, both built-in and user-defined, including all observer, turtle, and patch variables, from an external file named by the given string" ) 
( "in-cone" ,(concat (propertize "agentset " 'face 'italic) "in-cone " (propertize "distance " 'face 'italic) (propertize "angle " 'face 'italic) ) . 
"This reporter lets you give a turtle a 'cone of vision' in front of itself" ) 
( "in-<breed>-neighbor?" ,(concat "in-<breed>-neighbor? " (propertize "agent " 'face 'italic) ) . 
"Reports true if there is a directed link going from turtle to the caller" ) 
( "in-link-neighbor?" ,(concat "in-link-neighbor? " (propertize "turtle " 'face 'italic) ) . 
"Reports true if there is a directed link going from turtle to the caller" ) 
( "in-<breed>-neighbors" ,(concat "in-<breed>-neighbors " ) . 
"Reports the agentset of all the turtles that have directed links coming from them to the caller" ) 
( "in-link-neighbors" ,(concat "in-link-neighbors " ) . 
"Reports the agentset of all the turtles that have directed links coming from them to the caller" ) 
( "in-<breed>-from" ,(concat "in-<breed>-from " (propertize "turtle " 'face 'italic) ) . 
"Report the directed link from turtle to the caller" ) 
( "in-link-from" ,(concat "in-link-from " (propertize "turtle " 'face 'italic) ) . 
"Report the directed link from turtle to the caller" ) 
( "__includes" ,(concat "__includes " (propertize "[ " 'face 'italic) (propertize "filename " 'face 'italic) (propertize "... " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Causes external NetLogo source files (with the" ) 
( "in-radius" ,(concat (propertize "agentset " 'face 'italic) "in-radius " (propertize "number " 'face 'italic) ) . 
"Reports an agentset that includes only those agents from the original agentset whose distance from the caller is less than or equal to number" ) 
( "inspect" ,(concat "inspect " (propertize "agent " 'face 'italic) ) . 
"Opens an agent monitor for the given agent (turtle or patch)" ) 
( "int" ,(concat "int " (propertize "number " 'face 'italic) ) . 
"Reports the integer part of number -- any fractional part is discarded" ) 
( "is-agent?" ,(concat "is-agent? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-agentset?" ,(concat "is-agentset? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-boolean?" ,(concat "is-boolean? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-" ,(concat (propertize "is-<breed>? " 'face 'italic) (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-command-task?" ,(concat "is-command-task? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-directed-link?" ,(concat "is-directed-link? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-link?" ,(concat "is-link? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-link-set?" ,(concat "is-link-set? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-list?" ,(concat "is-list? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-number?" ,(concat "is-number? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-patch?" ,(concat "is-patch? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-patch-set?" ,(concat "is-patch-set? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-reporter-task?" ,(concat "is-reporter-task? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-string?" ,(concat "is-string? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-turtle?" ,(concat "is-turtle? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-turtle-set?" ,(concat "is-turtle-set? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "is-undirected-link?" ,(concat "is-undirected-link? " (propertize "value " 'face 'italic) ) . 
"Reports true if value is of the given type, false otherwise" ) 
( "item" ,(concat "item " (propertize "index " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"On lists, reports the value of the item in the given list with the given index" ) 
( "item" ,(concat "item " (propertize "index " 'face 'italic) (propertize "string " 'face 'italic) ) . 
"On lists, reports the value of the item in the given list with the given index" ) 
( "jump" ,(concat "jump " (propertize "number " 'face 'italic) ) . 
"The turtle moves forward by number units all at once (rather than one step at a time as with the forward command)" ) 
( "label" ,(concat "label " ) . 
"This is a built-in turtle or link variable" ) 
( "label-color" ,(concat "label-color " ) . 
"This is a built-in turtle or link variable" ) 
( "last" ,(concat "last " (propertize "list " 'face 'italic) ) . 
"On a list, reports the last item in the list" ) 
( "last" ,(concat "last " (propertize "string " 'face 'italic) ) . 
"On a list, reports the last item in the list" ) 
( "layout-circle" ,(concat "layout-circle " (propertize "agentset " 'face 'italic) (propertize "radius " 'face 'italic) ) . 
"Arranges the given turtles in a circle centered on the patch at the center of the world with the given radius" ) 
( "layout-circle" ,(concat "layout-circle " (propertize "list-of-turtles " 'face 'italic) (propertize "radius " 'face 'italic) ) . 
"Arranges the given turtles in a circle centered on the patch at the center of the world with the given radius" ) 
( "layout-radial" ,(concat "layout-radial " (propertize "turtle-set " 'face 'italic) (propertize "link-set " 'face 'italic) (propertize "root-agent " 'face 'italic) ) . 
"Arranges the turtles in turtle-set connected by links in link-set, in a radial tree layout, centered around the root-agent which is moved to the center of the world view" ) 
( "layout-spring" ,(concat "layout-spring " (propertize "turtle-set " 'face 'italic) (propertize "link-set
 " 'face 'italic) (propertize " " 'face 'italic) (propertize " " 'face 'italic) (propertize " " 'face 'italic) (propertize " " 'face 'italic) (propertize " " 'face 'italic) (propertize " " 'face 'italic) (propertize " " 'face 'italic) (propertize " " 'face 'italic) (propertize " " 'face 'italic) (propertize "spring-constant " 'face 'italic) (propertize "spring-length " 'face 'italic) (propertize "repulsion-constant " 'face 'italic) ) . 
"Arranges the turtles in turtle-set, as if the links in link-set are springs and the turtles are repelling each other" ) 
( "layout-tutte" ,(concat "layout-tutte " (propertize "turtle-set " 'face 'italic) (propertize "link-set " 'face 'italic) (propertize "radius " 'face 'italic) ) . 
"The turtles that are connected by links in link-set but not included in turtle-set are placed in a circle layout with the given radius" ) 
( "left" ,(concat "left " (propertize "number " 'face 'italic) ) . 
"The turtle turns left by number degrees" ) 
( "length" ,(concat "length " (propertize "list " 'face 'italic) ) . 
"Reports the number of items in the given list, or the number of characters in the given string" ) 
( "length" ,(concat "length " (propertize "string " 'face 'italic) ) . 
"Reports the number of items in the given list, or the number of characters in the given string" ) 
( "let" ,(concat "let " (propertize "variable " 'face 'italic) (propertize "value " 'face 'italic) ) . 
"Creates a new local variable and gives it the given value" ) 
( "link" ,(concat "link " (propertize "end1 " 'face 'italic) (propertize "end2 " 'face 'italic) (propertize "<breed> " 'face 'italic) (propertize "end1 " 'face 'italic) (propertize "end2 " 'face 'italic) ) . 
"Given the who numbers of the endpoints, reports the link connecting the turtles" ) 
( "link-heading" ,(concat "link-heading " ) . 
"Reports the heading in degrees (at least 0, less than 360) from end1 to end2 of the link" ) 
( "link-length" ,(concat "link-length " ) . 
"Reports the distance between the endpoints of the link" ) 
( "link-set" ,(concat "link-set " (propertize "value " 'face 'italic) ) . 
"Reports an agentset containing all of the links anywhere in any of the inputs" ) 
( "(link-set" ,(concat "(link-set " (propertize "value1 " 'face 'italic) (propertize "value2 " 'face 'italic) (propertize "...) " 'face 'italic) ) . 
"Reports an agentset containing all of the links anywhere in any of the inputs" ) 
( "link-shapes" ,(concat "link-shapes " ) . 
"Reports a list of strings containing all of the link shapes in the model" ) 
( "links" ,(concat "links " ) . 
"Reports the agentset consisting of all links" ) 
( "links-own" ,(concat "links-own " (propertize "[var1 " 'face 'italic) (propertize "...] " 'face 'italic) ) . 
"The links-own keyword, like the globals, breed, <breeds>-own, turtles-own, and patches-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "-own" ,(concat (propertize "<link-breeds>-own " 'face 'italic) (propertize "[var1 " 'face 'italic) (propertize "...] " 'face 'italic) ) . 
"The links-own keyword, like the globals, breed, <breeds>-own, turtles-own, and patches-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "list" ,(concat "list " (propertize "value1 " 'face 'italic) (propertize "value2 " 'face 'italic) ) . 
"The links-own keyword, like the globals, breed, <breeds>-own, turtles-own, and patches-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "(list" ,(concat "(list " (propertize "value1 " 'face 'italic) (propertize "...) " 'face 'italic) ) . 
"The links-own keyword, like the globals, breed, <breeds>-own, turtles-own, and patches-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "ln" ,(concat "ln " (propertize "number " 'face 'italic) ) . 
"The links-own keyword, like the globals, breed, <breeds>-own, turtles-own, and patches-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "log" ,(concat "log " (propertize "number " 'face 'italic) (propertize "base " 'face 'italic) ) . 
"The links-own keyword, like the globals, breed, <breeds>-own, turtles-own, and patches-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "loop" ,(concat "loop " (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"The links-own keyword, like the globals, breed, <breeds>-own, turtles-own, and patches-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "lput" ,(concat "lput " (propertize "value " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"The links-own keyword, like the globals, breed, <breeds>-own, turtles-own, and patches-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "list" ,(concat "list " (propertize "value1 " 'face 'italic) (propertize "value2 " 'face 'italic) ) . 
"Reports a list containing the given items" ) 
( "(list" ,(concat "(list " (propertize "value1 " 'face 'italic) (propertize "...) " 'face 'italic) ) . 
"Reports a list containing the given items" ) 
( "ln" ,(concat "ln " (propertize "number " 'face 'italic) ) . 
"Reports the natural logarithm of number, that is, the logarithm to the base e (2" ) 
( "log" ,(concat "log " (propertize "number " 'face 'italic) (propertize "base " 'face 'italic) ) . 
"Reports the logarithm of number in base base" ) 
( "loop" ,(concat "loop " (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Repeats the commands forever, or until the enclosing procedure exits through use of the stop or report commands" ) 
( "lput" ,(concat "lput " (propertize "value " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"Adds value to the end of a list and reports the new list" ) 
( "map" ,(concat "map " (propertize "reporter-task " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"With a single list, the given task is run for each item in the list, and a list of the results is collected and reported" ) 
( "(map" ,(concat "(map " (propertize "reporter-task " 'face 'italic) (propertize "list1 " 'face 'italic) (propertize "...) " 'face 'italic) ) . 
"With a single list, the given task is run for each item in the list, and a list of the results is collected and reported" ) 
( "max" ,(concat "max " (propertize "list " 'face 'italic) ) . 
"Reports the maximum number value in the list" ) 
( "max-n-of" ,(concat "max-n-of " (propertize "number " 'face 'italic) (propertize "agentset " 'face 'italic) (propertize "[reporter] " 'face 'italic) ) . 
"Reports an agentset containing number agents from agentset with the highest values of reporter" ) 
( "max-one-of" ,(concat "max-one-of " (propertize "agentset " 'face 'italic) (propertize "[reporter] " 'face 'italic) ) . 
"Reports the agent in the agentset that has the highest value for the given reporter" ) 
( "max-pxcor" ,(concat "max-pxcor " ) . 
"These reporters give the maximum x-coordinate and maximum y-coordinate, (respectively) for patches, which determines the size of the world" ) 
( "max-pycor" ,(concat "max-pycor " ) . 
"These reporters give the maximum x-coordinate and maximum y-coordinate, (respectively) for patches, which determines the size of the world" ) 
( "mean" ,(concat "mean " (propertize "list " 'face 'italic) ) . 
"Reports the statistical mean of the numeric items in the given list" ) 
( "median" ,(concat "median " (propertize "list " 'face 'italic) ) . 
"Reports the statistical median of the numeric items of the given list" ) 
( "member?" ,(concat "member? " (propertize "value " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"For a list, reports true if the given value appears in the given list, otherwise reports false" ) 
( "member?" ,(concat "member? " (propertize "string1 " 'face 'italic) (propertize "string2 " 'face 'italic) ) . 
"For a list, reports true if the given value appears in the given list, otherwise reports false" ) 
( "member?" ,(concat "member? " (propertize "agent " 'face 'italic) (propertize "agentset " 'face 'italic) ) . 
"For a list, reports true if the given value appears in the given list, otherwise reports false" ) 
( "min" ,(concat "min " (propertize "list " 'face 'italic) ) . 
"Reports the minimum number value in the list" ) 
( "min-n-of" ,(concat "min-n-of " (propertize "number " 'face 'italic) (propertize "agentset " 'face 'italic) (propertize "[reporter] " 'face 'italic) ) . 
"Reports an agentset containing number agents from agentset with the lowest values of reporter" ) 
( "min-one-of" ,(concat "min-one-of " (propertize "agentset " 'face 'italic) (propertize "[reporter] " 'face 'italic) ) . 
"Reports a random agent in the agentset that reports the lowest value for the given reporter" ) 
( "min-pxcor" ,(concat "min-pxcor " ) . 
"These reporters give the minimum x-coordinate and minimum y-coordinate, (respectively) for patches, which determines the size of the world" ) 
( "min-pycor" ,(concat "min-pycor " ) . 
"These reporters give the minimum x-coordinate and minimum y-coordinate, (respectively) for patches, which determines the size of the world" ) 
( "mod" ,(concat (propertize "number1 " 'face 'italic) "mod " (propertize "number2 " 'face 'italic) ) . 
"Reports number1 modulo number2: that is, the residue of number1 (mod number2)" ) 
( "modes" ,(concat "modes " (propertize "list " 'face 'italic) ) . 
"Reports a list of the most common item or items in list" ) 
( "mouse-down?" ,(concat "mouse-down? " ) . 
"Reports true if the mouse button is down, false otherwise" ) 
( "mouse-inside?" ,(concat "mouse-inside? " ) . 
"Reports true if the mouse pointer is inside the current view, false otherwise" ) 
( "mouse-xcor" ,(concat "mouse-xcor " ) . 
"Reports the x or y coordinate of the mouse in the 2D view" ) 
( "mouse-ycor" ,(concat "mouse-ycor " ) . 
"Reports the x or y coordinate of the mouse in the 2D view" ) 
( "move-to" ,(concat "move-to " (propertize "agent " 'face 'italic) ) . 
"The turtle sets its x and y coordinates to be the same as the given agent's" ) 
( "movie-cancel" ,(concat "movie-cancel " ) . 
"Cancels the current movie" ) 
( "movie-close" ,(concat "movie-close " ) . 
"Stops the recording of the current movie" ) 
( "movie-grab-view" ,(concat "movie-grab-view " ) . 
"Adds an image of the current view (2D or 3D) or the interface panel to the current movie" ) 
( "movie-grab-interface" ,(concat "movie-grab-interface " ) . 
"Adds an image of the current view (2D or 3D) or the interface panel to the current movie" ) 
( "movie-set-frame-rate" ,(concat "movie-set-frame-rate " (propertize "frame-rate " 'face 'italic) ) . 
"Sets the frame rate of the current movie" ) 
( "movie-start" ,(concat "movie-start " (propertize "filename " 'face 'italic) ) . 
"Creates a new movie" ) 
( "movie-status" ,(concat "movie-status " ) . 
"Reports a string describing the current movie" ) 
( "my-<breeds>" ,(concat "my-<breeds> " ) . 
"Reports an agentset of all links connected to the caller, regardless of directedness" ) 
( "my-links" ,(concat "my-links " ) . 
"Reports an agentset of all links connected to the caller, regardless of directedness" ) 
( "my-in-<breeds>" ,(concat "my-in-<breeds> " ) . 
"Reports an agentset of all the directed links coming in from other nodes to the caller" ) 
( "my-in-links" ,(concat "my-in-links " ) . 
"Reports an agentset of all the directed links coming in from other nodes to the caller" ) 
( "my-out-<breeds>" ,(concat "my-out-<breeds> " ) . 
"Reports an agentset of all the directed links going out from the caller to other nodes" ) 
( "my-out-links" ,(concat "my-out-links " ) . 
"Reports an agentset of all the directed links going out from the caller to other nodes" ) 
( "myself" ,(concat "myself " ) . 
"'self' and 'myself' are very different" ) 
( "n-of" ,(concat "n-of " (propertize "size " 'face 'italic) (propertize "agentset " 'face 'italic) ) . 
"From an agentset, reports an agentset of size size randomly chosen from the input set, with no repeats" ) 
( "n-of" ,(concat "n-of " (propertize "size " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"From an agentset, reports an agentset of size size randomly chosen from the input set, with no repeats" ) 
( "n-values" ,(concat "n-values " (propertize "size " 'face 'italic) (propertize "reporter-task " 'face 'italic) ) . 
"Reports a list of length size containing values computed by repeatedly running the task" ) 
( "neighbors" ,(concat "neighbors " ) . 
"Reports an agentset containing the 8 surrounding patches (neighbors) or 4 surrounding patches (neighbors4)" ) 
( "neighbors4" ,(concat "neighbors4 " ) . 
"Reports an agentset containing the 8 surrounding patches (neighbors) or 4 surrounding patches (neighbors4)" ) 
( "<breed>-neighbors" ,(concat "<breed>-neighbors " ) . 
"Reports the agentset of all turtles found at the other end of undirected links connected to this turtle" ) 
( "link-neighbors" ,(concat "link-neighbors " ) . 
"Reports the agentset of all turtles found at the other end of undirected links connected to this turtle" ) 
( "<breed>-neighbor?" ,(concat "<breed>-neighbor? " (propertize "turtle " 'face 'italic) ) . 
"Reports true if there is an undirected link between turtle and the caller" ) 
( "link-neighbor?" ,(concat "link-neighbor? " (propertize "turtle " 'face 'italic) ) . 
"Reports true if there is an undirected link between turtle and the caller" ) 
( "netlogo-applet?" ,(concat "netlogo-applet? " ) . 
"Reports true if the model is running as an applet" ) 
( "netlogo-version" ,(concat "netlogo-version " ) . 
"Reports a string containing the version number of the NetLogo you are running" ) 
( "netlogo-web?" ,(concat "netlogo-web? " ) . 
"Reports true if the model is running in NetLogo Web" ) 
( "new-seed" ,(concat "new-seed " ) . 
"Reports a number suitable for seeding the random number generator" ) 
( "no-display" ,(concat "no-display " ) . 
"Turns off all updates to the current view until the display command is issued" ) 
( "nobody" ,(concat "nobody " ) . 
"This is a special value which some primitives such as turtle, one-of, max-one-of, etc" ) 
( "no-links" ,(concat "no-links " ) . 
"Reports an empty link agentset" ) 
( "no-patches" ,(concat "no-patches " ) . 
"Reports an empty patch agentset" ) 
( "not" ,(concat "not " (propertize "boolean " 'face 'italic) ) . 
"Reports true if boolean is false, otherwise reports false" ) 
( "no-turtles" ,(concat "no-turtles " ) . 
"Reports an empty turtle agentset" ) 
( "[" ,(concat (propertize "[reporter] " 'face 'italic) (propertize "of " 'face 'italic) (propertize "agent " 'face 'italic) ) . 
"For an agent, reports the value of the reporter for that agent (turtle or patch)" ) 
( "[" ,(concat (propertize "[reporter] " 'face 'italic) (propertize "of " 'face 'italic) (propertize "agentset " 'face 'italic) ) . 
"For an agent, reports the value of the reporter for that agent (turtle or patch)" ) 
( "one-of" ,(concat "one-of " (propertize "agentset " 'face 'italic) ) . 
"From an agentset, reports a random agent" ) 
( "one-of" ,(concat "one-of " (propertize "list " 'face 'italic) ) . 
"From an agentset, reports a random agent" ) 
( "or" ,(concat (propertize "boolean1 " 'face 'italic) "or " (propertize "boolean2 " 'face 'italic) ) . 
"Reports true if either boolean1 or boolean2, or both, is true" ) 
( "other" ,(concat "other " (propertize "agentset " 'face 'italic) ) . 
"Reports an agentset which is the same as the input agentset but omits this agent" ) 
( "other-end" ,(concat "other-end " ) . 
"If run by a turtle, reports the turtle at the other end of the asking link" ) 
( "out-<breed>-neighbor?" ,(concat "out-<breed>-neighbor? " (propertize "turtle " 'face 'italic) ) . 
"Reports true if there is a directed link going from the caller to turtle" ) 
( "out-link-neighbor?" ,(concat "out-link-neighbor? " (propertize "turtle " 'face 'italic) ) . 
"Reports true if there is a directed link going from the caller to turtle" ) 
( "out-<breed>-neighbors" ,(concat "out-<breed>-neighbors " ) . 
"Reports the agentset of all the turtles that have directed links from the caller" ) 
( "out-link-neighbors" ,(concat "out-link-neighbors " ) . 
"Reports the agentset of all the turtles that have directed links from the caller" ) 
( "out-<breed>-to" ,(concat "out-<breed>-to " (propertize "turtle " 'face 'italic) ) . 
"Reports the directed link from the caller to turtle" ) 
( "out-link-to" ,(concat "out-link-to " (propertize "turtle " 'face 'italic) ) . 
"Reports the directed link from the caller to turtle" ) 
( "output-print" ,(concat "output-print " (propertize "value " 'face 'italic) ) . 
"These commands are the same as the print, show, type, and write commands except that value is printed in the model's output area, instead of in the Command Center" ) 
( "output-show" ,(concat "output-show " (propertize "value " 'face 'italic) ) . 
"These commands are the same as the print, show, type, and write commands except that value is printed in the model's output area, instead of in the Command Center" ) 
( "output-type" ,(concat "output-type " (propertize "value " 'face 'italic) ) . 
"These commands are the same as the print, show, type, and write commands except that value is printed in the model's output area, instead of in the Command Center" ) 
( "output-write" ,(concat "output-write " (propertize "value " 'face 'italic) ) . 
"These commands are the same as the print, show, type, and write commands except that value is printed in the model's output area, instead of in the Command Center" ) 
( "patch" ,(concat "patch " (propertize "xcor " 'face 'italic) (propertize "ycor " 'face 'italic) ) . 
"Given the x and y coordinates of a point, reports the patch containing that point" ) 
( "patch-ahead" ,(concat "patch-ahead " (propertize "distance " 'face 'italic) ) . 
"Reports the single patch that is the given distance 'ahead' of this turtle, that is, along the turtle's current heading" ) 
( "patch-at" ,(concat "patch-at " (propertize "dx " 'face 'italic) (propertize "dy " 'face 'italic) ) . 
"Reports the patch at (dx, dy) from the caller, that is, the patch containing the point dx east and dy patches north of this agent" ) 
( "patch-at-heading-and-distance" ,(concat "patch-at-heading-and-distance " (propertize "heading " 'face 'italic) (propertize "distance " 'face 'italic) ) . 
"patch-at-heading-and-distance reports the single patch that is the given distance from this turtle or patch, along the given absolute heading" ) 
( "patch-here" ,(concat "patch-here " ) . 
"patch-here reports the patch under the turtle" ) 
( "patch-left-and-ahead" ,(concat "patch-left-and-ahead " (propertize "angle " 'face 'italic) (propertize "distance " 'face 'italic) ) . 
"Reports the single patch that is the given distance from this turtle, in the direction turned left or right the given angle (in degrees) from the turtle's current heading" ) 
( "patch-right-and-ahead" ,(concat "patch-right-and-ahead " (propertize "angle " 'face 'italic) (propertize "distance " 'face 'italic) ) . 
"Reports the single patch that is the given distance from this turtle, in the direction turned left or right the given angle (in degrees) from the turtle's current heading" ) 
( "patch-set" ,(concat "patch-set " (propertize "value1 " 'face 'italic) ) . 
"Reports an agentset containing all of the patches anywhere in any of the inputs" ) 
( "(patch-set" ,(concat "(patch-set " (propertize "value1 " 'face 'italic) (propertize "value2 " 'face 'italic) (propertize "...) " 'face 'italic) ) . 
"Reports an agentset containing all of the patches anywhere in any of the inputs" ) 
( "patch-size" ,(concat "patch-size " ) . 
"Reports the size of the patches in the view in pixels" ) 
( "patches" ,(concat "patches " ) . 
"Reports the agentset consisting of all patches" ) 
( "patches-own" ,(concat "patches-own " (propertize "[var1 " 'face 'italic) (propertize "...] " 'face 'italic) ) . 
"This keyword, like the globals, breed, <breed>-own, and turtles-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "pcolor" ,(concat "pcolor " ) . 
"This is a built-in patch variable" ) 
( "pen-down" ,(concat "pen-down " ) . 
"The turtle changes modes between drawing lines, removing lines or neither" ) 
( "pen-erase" ,(concat "pen-erase " ) . 
"The turtle changes modes between drawing lines, removing lines or neither" ) 
( "pen-up" ,(concat "pen-up " ) . 
"The turtle changes modes between drawing lines, removing lines or neither" ) 
( "plabel" ,(concat "plabel " ) . 
"This is a built-in patch variable" ) 
( "plabel-color" ,(concat "plabel-color " ) . 
"This is a built-in patch variable" ) 
( "plot" ,(concat "plot " (propertize "number " 'face 'italic) ) . 
"Increments the x-value of the plot pen by plot-pen-interval, then plots a point at the updated x-value and a y-value of number" ) 
( "plot-name" ,(concat "plot-name " ) . 
"Reports the name of the current plot (a string)" ) 
( "plot-pen-exists?" ,(concat "plot-pen-exists? " (propertize "string " 'face 'italic) ) . 
"Reports true if a plot pen with the given name is defined in the current plot" ) 
( "plot-pen-down" ,(concat "plot-pen-down " ) . 
"Puts down (or up) the current plot-pen, so that it draws (or doesn't)" ) 
( "plot-pen-up" ,(concat "plot-pen-up " ) . 
"Puts down (or up) the current plot-pen, so that it draws (or doesn't)" ) 
( "plot-pen-reset" ,(concat "plot-pen-reset " ) . 
"Clears everything the current plot pen has drawn, moves it to (0,0), and puts it down" ) 
( "plotxy" ,(concat "plotxy " (propertize "number1 " 'face 'italic) (propertize "number2 " 'face 'italic) ) . 
"Moves the current plot pen to the point with coordinates (number1, number2)" ) 
( "plot-x-min" ,(concat "plot-x-min " ) . 
"Reports the minimum or maximum value on the x or y axis of the current plot" ) 
( "plot-x-max" ,(concat "plot-x-max " ) . 
"Reports the minimum or maximum value on the x or y axis of the current plot" ) 
( "plot-y-min" ,(concat "plot-y-min " ) . 
"Reports the minimum or maximum value on the x or y axis of the current plot" ) 
( "plot-y-max" ,(concat "plot-y-max " ) . 
"Reports the minimum or maximum value on the x or y axis of the current plot" ) 
( "position" ,(concat "position " (propertize "item " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"On a list, reports the first position of item in list, or false if it does not appear" ) 
( "position" ,(concat "position " (propertize "string1 " 'face 'italic) (propertize "string2 " 'face 'italic) ) . 
"On a list, reports the first position of item in list, or false if it does not appear" ) 
( "precision" ,(concat "precision " (propertize "number " 'face 'italic) (propertize "places " 'face 'italic) ) . 
"Reports number rounded to places decimal places" ) 
( "print" ,(concat "print " (propertize "value " 'face 'italic) ) . 
"Prints value in the Command Center, followed by a carriage return" ) 
( "pxcor" ,(concat "pxcor " ) . 
"These are built-in patch variables" ) 
( "pycor" ,(concat "pycor " ) . 
"These are built-in patch variables" ) 
( "random" ,(concat "random " (propertize "number " 'face 'italic) ) . 
"If number is positive, reports a random integer greater than or equal to 0, but strictly less than number" ) 
( "random-float" ,(concat "random-float " (propertize "number " 'face 'italic) ) . 
"If number is positive, reports a random floating point number greater than or equal to 0 but strictly less than number" ) 
( "random-exponential" ,(concat "random-exponential " (propertize "mean " 'face 'italic) ) . 
"Reports an accordingly distributed random number with the mean and, in the case of the normal distribution, the standard-deviation" ) 
( "random-gamma" ,(concat "random-gamma " (propertize "alpha " 'face 'italic) (propertize "lambda " 'face 'italic) ) . 
"Reports an accordingly distributed random number with the mean and, in the case of the normal distribution, the standard-deviation" ) 
( "random-normal" ,(concat "random-normal " (propertize "mean " 'face 'italic) (propertize "standard-deviation " 'face 'italic) ) . 
"Reports an accordingly distributed random number with the mean and, in the case of the normal distribution, the standard-deviation" ) 
( "random-poisson" ,(concat "random-poisson " (propertize "mean " 'face 'italic) ) . 
"Reports an accordingly distributed random number with the mean and, in the case of the normal distribution, the standard-deviation" ) 
( "random-pxcor" ,(concat "random-pxcor " ) . 
"Reports a random integer ranging from min-pxcor (or -y) to max-pxcor (or -y) inclusive" ) 
( "random-pycor" ,(concat "random-pycor " ) . 
"Reports a random integer ranging from min-pxcor (or -y) to max-pxcor (or -y) inclusive" ) 
( "random-seed" ,(concat "random-seed " (propertize "number " 'face 'italic) ) . 
"Sets the seed of the pseudo-random number generator to the integer part of number" ) 
( "random-xcor" ,(concat "random-xcor " ) . 
"Reports a random floating point number from the allowable range of turtle coordinates along the given axis, x or y" ) 
( "random-ycor" ,(concat "random-ycor " ) . 
"Reports a random floating point number from the allowable range of turtle coordinates along the given axis, x or y" ) 
( "read-from-string" ,(concat "read-from-string " (propertize "string " 'face 'italic) ) . 
"Interprets the given string as if it had been typed in the Command Center, and reports the resulting value" ) 
( "reduce" ,(concat "reduce " (propertize "reporter-task " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"Reduces a list from left to right using the given task, resulting in a single value" ) 
( "remainder" ,(concat "remainder " (propertize "number1 " 'face 'italic) (propertize "number2 " 'face 'italic) ) . 
"Reports the remainder when number1 is divided by number2" ) 
( "remove" ,(concat "remove " (propertize "item " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"For a list, reports a copy of list with all instances of item removed" ) 
( "remove" ,(concat "remove " (propertize "string1 " 'face 'italic) (propertize "string2 " 'face 'italic) ) . 
"For a list, reports a copy of list with all instances of item removed" ) 
( "remove-duplicates" ,(concat "remove-duplicates " (propertize "list " 'face 'italic) ) . 
"Reports a copy of list with all duplicate items removed" ) 
( "remove-item" ,(concat "remove-item " (propertize "index " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"For a list, reports a copy of list with the item at the given index removed" ) 
( "remove-item" ,(concat "remove-item " (propertize "index " 'face 'italic) (propertize "string " 'face 'italic) ) . 
"For a list, reports a copy of list with the item at the given index removed" ) 
( "repeat" ,(concat "repeat " (propertize "number " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Runs commands number times" ) 
( "replace-item" ,(concat "replace-item " (propertize "index " 'face 'italic) (propertize "list " 'face 'italic) (propertize "value " 'face 'italic) ) . 
"On a list, replaces an item in that list" ) 
( "replace-item" ,(concat "replace-item " (propertize "index " 'face 'italic) (propertize "string1 " 'face 'italic) (propertize "string2 " 'face 'italic) ) . 
"On a list, replaces an item in that list" ) 
( "report" ,(concat "report " (propertize "value " 'face 'italic) ) . 
"Immediately exits from the current to-report procedure and reports value as the result of that procedure" ) 
( "reset-perspective" ,(concat "reset-perspective " ) . 
"The observer stops watching, following, or riding any turtles (or patches)" ) 
( "reset-ticks" ,(concat "reset-ticks " ) . 
"Resets the tick counter to zero, sets up all plots, then updates all plots (so that the initial state of the world is plotted)" ) 
( "reset-timer" ,(concat "reset-timer " ) . 
"Resets the timer to zero seconds" ) 
( "resize-world" ,(concat "resize-world " (propertize "min-pxcor " 'face 'italic) (propertize "max-pxcor " 'face 'italic) (propertize "min-pycor " 'face 'italic) (propertize "max-pycor " 'face 'italic) ) . 
"Changes the size of the patch grid" ) 
( "reverse" ,(concat "reverse " (propertize "list " 'face 'italic) ) . 
"Reports a reversed copy of the given list or string" ) 
( "reverse" ,(concat "reverse " (propertize "string " 'face 'italic) ) . 
"Reports a reversed copy of the given list or string" ) 
( "rgb" ,(concat "rgb " (propertize "red " 'face 'italic) (propertize "green " 'face 'italic) (propertize "blue " 'face 'italic) ) . 
"Reports a RGB list when given three numbers describing an RGB color" ) 
( "ride" ,(concat "ride " (propertize "turtle " 'face 'italic) ) . 
"Set the perspective to turtle" ) 
( "ride-me" ,(concat "ride-me " ) . 
"Asks the observer to ride this turtle" ) 
( "right" ,(concat "right " (propertize "number " 'face 'italic) ) . 
"The turtle turns right by number degrees" ) 
( "round" ,(concat "round " (propertize "number " 'face 'italic) ) . 
"Reports the integer nearest to number" ) 
( "run" ,(concat "run " (propertize "command-task " 'face 'italic) ) . 
"The run form expects a command task or a string containing commands" ) 
( "(run" ,(concat "(run " (propertize "command-task " 'face 'italic) (propertize "input1 " 'face 'italic) (propertize "...) " 'face 'italic) ) . 
"The run form expects a command task or a string containing commands" ) 
( "run" ,(concat "run " (propertize "string " 'face 'italic) ) . 
"The run form expects a command task or a string containing commands" ) 
( "runresult" ,(concat "runresult " (propertize "reporter-task " 'face 'italic) ) . 
"The run form expects a command task or a string containing commands" ) 
( "(runresult" ,(concat "(runresult " (propertize "reporter-task " 'face 'italic) (propertize "input1 " 'face 'italic) (propertize "...) " 'face 'italic) ) . 
"The run form expects a command task or a string containing commands" ) 
( "runresult" ,(concat "runresult " (propertize "string " 'face 'italic) ) . 
"The run form expects a command task or a string containing commands" ) 
( "scale-color" ,(concat "scale-color " (propertize "color " 'face 'italic) (propertize "number " 'face 'italic) (propertize "range1 " 'face 'italic) (propertize "range2 " 'face 'italic) ) . 
"Reports a shade of color proportional to the value of number" ) 
( "self" ,(concat "self " ) . 
"Reports this turtle, patch, or link" ) 
( ";" ,(concat "; " (propertize "comments " 'face 'italic) ) . 
"After a semicolon, the rest of the line is ignored" ) 
( "sentence" ,(concat "sentence " (propertize "value1 " 'face 'italic) (propertize "value2 " 'face 'italic) ) . 
"Makes a list out of the values" ) 
( "(sentence" ,(concat "(sentence " (propertize "value1 " 'face 'italic) (propertize "...) " 'face 'italic) ) . 
"Makes a list out of the values" ) 
( "set" ,(concat "set " (propertize "variable " 'face 'italic) (propertize "value " 'face 'italic) ) . 
"Sets variable to the given value" ) 
( "set-current-directory" ,(concat "set-current-directory " (propertize "string " 'face 'italic) ) . 
"Sets the current directory that is used by the primitives file-delete, file-exists?, and file-open" ) 
( "set-current-plot" ,(concat "set-current-plot " (propertize "plotname " 'face 'italic) ) . 
"Sets the current plot to the plot with the given name (a string)" ) 
( "set-current-plot-pen" ,(concat "set-current-plot-pen " (propertize "penname " 'face 'italic) ) . 
"The current plot's current pen is set to the pen named penname (a string)" ) 
( "set-default-shape" ,(concat "set-default-shape " (propertize "turtles " 'face 'italic) (propertize "string " 'face 'italic) ) . 
"Specifies a default initial shape for all turtles or links, or for a particular breed of turtles or links" ) 
( "set-default-shape" ,(concat "set-default-shape " (propertize "links " 'face 'italic) (propertize "string " 'face 'italic) ) . 
"Specifies a default initial shape for all turtles or links, or for a particular breed of turtles or links" ) 
( "set-default-shape" ,(concat "set-default-shape " (propertize "breed " 'face 'italic) (propertize "string " 'face 'italic) ) . 
"Specifies a default initial shape for all turtles or links, or for a particular breed of turtles or links" ) 
( "set-histogram-num-bars" ,(concat "set-histogram-num-bars " (propertize "number " 'face 'italic) ) . 
"Set the current plot pen's plot interval so that, given the current x range for the plot, there would be number number of bars drawn if the histogram command is called" ) 
( "__set-line-thickness" ,(concat "__set-line-thickness " (propertize "number " 'face 'italic) ) . 
"Specifies the thickness of lines and outlined elements in the turtle's shape" ) 
( "set-patch-size" ,(concat "set-patch-size " (propertize "size " 'face 'italic) ) . 
"Sets the size of the patches of the view in pixels" ) 
( "set-plot-pen-color" ,(concat "set-plot-pen-color " (propertize "number " 'face 'italic) ) . 
"Sets the color of the current plot pen to number" ) 
( "set-plot-pen-interval" ,(concat "set-plot-pen-interval " (propertize "number " 'face 'italic) ) . 
"Tells the current plot pen to move a distance of number in the x direction during each use of the plot command" ) 
( "set-plot-pen-mode" ,(concat "set-plot-pen-mode " (propertize "number " 'face 'italic) ) . 
"Sets the mode the current plot pen draws in to number" ) 
( "setup-plots" ,(concat "setup-plots " ) . 
"For each plot, runs that plot's setup commands, including the setup code for any pens in the plot" ) 
( "set-plot-x-range" ,(concat "set-plot-x-range " (propertize "min " 'face 'italic) (propertize "max " 'face 'italic) ) . 
"Sets the minimum and maximum values of the x or y axis of the current plot" ) 
( "set-plot-y-range" ,(concat "set-plot-y-range " (propertize "min " 'face 'italic) (propertize "max " 'face 'italic) ) . 
"Sets the minimum and maximum values of the x or y axis of the current plot" ) 
( "setxy" ,(concat "setxy " (propertize "x " 'face 'italic) (propertize "y " 'face 'italic) ) . 
"The turtle sets its x-coordinate to x and its y-coordinate to y" ) 
( "shade-of?" ,(concat "shade-of? " (propertize "color1 " 'face 'italic) (propertize "color2 " 'face 'italic) ) . 
"Reports true if both colors are shades of one another, false otherwise" ) 
( "shape" ,(concat "shape " ) . 
"This is a built-in turtle and link variable" ) 
( "shapes" ,(concat "shapes " ) . 
"Reports a list of strings containing all of the turtle shapes in the model" ) 
( "show" ,(concat "show " (propertize "value " 'face 'italic) ) . 
"Prints value in the Command Center, preceded by this agent, and followed by a carriage return" ) 
( "show-turtle" ,(concat "show-turtle " ) . 
"The turtle becomes visible again" ) 
( "show-link" ,(concat "show-link " ) . 
"The link becomes visible again" ) 
( "shuffle" ,(concat "shuffle " (propertize "list " 'face 'italic) ) . 
"Reports a new list containing the same items as the input list, but in randomized order" ) 
( "sin" ,(concat "sin " (propertize "number " 'face 'italic) ) . 
"Reports the sine of the given angle" ) 
( "size" ,(concat "size " ) . 
"This is a built-in turtle variable" ) 
( "sort" ,(concat "sort " (propertize "list " 'face 'italic) ) . 
"Reports a sorted list of numbers, strings, or agents" ) 
( "sort" ,(concat "sort " (propertize "agentset " 'face 'italic) ) . 
"Reports a sorted list of numbers, strings, or agents" ) 
( "sort-by" ,(concat "sort-by " (propertize "reporter-task " 'face 'italic) (propertize "list " 'face 'italic) ) . 
"If the input is a list, reports a new list containing the same items as the input list, in a sorted order defined by the boolean reporter task" ) 
( "sort-by" ,(concat "sort-by " (propertize "reporter-task " 'face 'italic) (propertize "agentset " 'face 'italic) ) . 
"If the input is a list, reports a new list containing the same items as the input list, in a sorted order defined by the boolean reporter task" ) 
( "sort-on" ,(concat "sort-on " (propertize "[reporter] " 'face 'italic) (propertize "agentset " 'face 'italic) ) . 
"Reports a list of agents, sorted according to each agent's value for reporter" ) 
( "sprout" ,(concat "sprout " (propertize "number " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Creates number new turtles on the current patch" ) 
( "sprout-" ,(concat (propertize "sprout-<breeds> " 'face 'italic) (propertize "number " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Creates number new turtles on the current patch" ) 
( "sqrt" ,(concat "sqrt " (propertize "number " 'face 'italic) ) . 
"Reports the square root of number" ) 
( "stamp" ,(concat "stamp " ) . 
"This turtle or link leaves an image of its shape in the drawing at its current location" ) 
( "stamp-erase" ,(concat "stamp-erase " ) . 
"This turtle or link removes any pixels below it in the drawing inside the bounds of its shape" ) 
( "standard-deviation" ,(concat "standard-deviation " (propertize "list " 'face 'italic) ) . 
"Reports the sample standard deviation of a list of numbers" ) 
( "startup" ,(concat "startup " ) . 
"User-defined procedure which, if it exists, will be called when a model is first loaded in the NetLogo application" ) 
( "stop" ,(concat "stop " ) . 
"This agent exits immediately from the enclosing procedure, ask, or ask-like construct (e" ) 
( "stop-inspecting" ,(concat "stop-inspecting " (propertize "agent " 'face 'italic) ) . 
"Closes the agent monitor for the given agent (turtle or patch)" ) 
( "stop-inspecting-dead-agents" ,(concat "stop-inspecting-dead-agents " ) . 
"Closes all agent monitors for dead agents" ) 
( "subject" ,(concat "subject " ) . 
"Reports the turtle (or patch) that the observer is currently watching, following, or riding" ) 
( "sublist" ,(concat "sublist " (propertize "list " 'face 'italic) (propertize "position1 " 'face 'italic) (propertize "position2 " 'face 'italic) ) . 
"Reports just a section of the given list or string, ranging between the first position (inclusive) and the second position (exclusive)" ) 
( "substring" ,(concat "substring " (propertize "string " 'face 'italic) (propertize "position1 " 'face 'italic) (propertize "position2 " 'face 'italic) ) . 
"Reports just a section of the given list or string, ranging between the first position (inclusive) and the second position (exclusive)" ) 
( "subtract-headings" ,(concat "subtract-headings " (propertize "heading1 " 'face 'italic) (propertize "heading2 " 'face 'italic) ) . 
"Computes the difference between the given headings, that is, the number of degrees in the smallest angle by which heading2 could be rotated to produce heading1" ) 
( "sum" ,(concat "sum " (propertize "list " 'face 'italic) ) . 
"Reports the sum of the items in the list" ) 
( "tan" ,(concat "tan " (propertize "number " 'face 'italic) ) . 
"Reports the tangent of the given angle" ) 
( "task" ,(concat "task " (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Creates and reports a task, either a command task or a reporter task, depending on the input" ) 
( "task" ,(concat "task " (propertize "[ " 'face 'italic) (propertize "reporter " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"Creates and reports a task, either a command task or a reporter task, depending on the input" ) 
( "task" ,(concat "task " (propertize "command-name " 'face 'italic) ) . 
"Creates and reports a task, either a command task or a reporter task, depending on the input" ) 
( "task" ,(concat "task " (propertize "reporter-name " 'face 'italic) ) . 
"Creates and reports a task, either a command task or a reporter task, depending on the input" ) 
( "thickness" ,(concat "thickness " ) . 
"This is a built-in link variable" ) 
( "tick" ,(concat "tick " ) . 
"Advances the tick counter by one and updates all plots" ) 
( "tick-advance" ,(concat "tick-advance " (propertize "number " 'face 'italic) ) . 
"Advances the tick counter by number" ) 
( "ticks" ,(concat "ticks " ) . 
"Reports the current value of the tick counter" ) 
( "tie" ,(concat "tie " ) . 
"Ties end1 and end2 of the link together" ) 
( "tie-mode" ,(concat "tie-mode " ) . 
"This is a built-in link variable" ) 
( "timer" ,(concat "timer " ) . 
"Reports how many seconds have passed since the command reset-timer was last run (or since NetLogo started)" ) 
( "to" ,(concat "to " (propertize "procedure-name " 'face 'italic) ) . 
"Used to begin a command procedure" ) 
( "to" ,(concat "to " (propertize "procedure-name " 'face 'italic) (propertize "[input1 " 'face 'italic) (propertize "...] " 'face 'italic) ) . 
"Used to begin a command procedure" ) 
( "to-report" ,(concat "to-report " (propertize "procedure-name " 'face 'italic) ) . 
"Used to begin a reporter procedure" ) 
( "to-report" ,(concat "to-report " (propertize "procedure-name " 'face 'italic) (propertize "[input1 " 'face 'italic) (propertize "...] " 'face 'italic) ) . 
"Used to begin a reporter procedure" ) 
( "towards" ,(concat "towards " (propertize "agent " 'face 'italic) ) . 
"Reports the heading from this agent to the given agent" ) 
( "towardsxy" ,(concat "towardsxy " (propertize "x " 'face 'italic) (propertize "y " 'face 'italic) ) . 
"Reports the heading from the turtle or patch towards the point (x,y)" ) 
( "turtle" ,(concat "turtle " (propertize "number " 'face 'italic) (propertize "<breed> " 'face 'italic) (propertize "number " 'face 'italic) ) . 
"Reports the turtle with the given who number, or nobody if there is no such turtle" ) 
( "turtle-set" ,(concat "turtle-set " (propertize "value1 " 'face 'italic) ) . 
"Reports an agentset containing all of the turtles anywhere in any of the inputs" ) 
( "(turtle-set" ,(concat "(turtle-set " (propertize "value1 " 'face 'italic) (propertize "value2 " 'face 'italic) (propertize "...) " 'face 'italic) ) . 
"Reports an agentset containing all of the turtles anywhere in any of the inputs" ) 
( "turtles" ,(concat "turtles " ) . 
"Reports the agentset consisting of all turtles" ) 
( "turtles-at" ,(concat "turtles-at " (propertize "dx " 'face 'italic) (propertize "dy " 'face 'italic) ) . 
"Reports an agentset containing the turtles on the patch (dx, dy) from the caller" ) 
( "-at" ,(concat (propertize "<breeds>-at " 'face 'italic) (propertize "dx " 'face 'italic) (propertize "dy " 'face 'italic) ) . 
"Reports an agentset containing the turtles on the patch (dx, dy) from the caller" ) 
( "turtles-here" ,(concat "turtles-here " ) . 
"Reports an agentset containing all the turtles on the caller's patch (including the caller itself if it's a turtle)" ) 
( "-here" ,(concat (propertize "<breeds>-here " 'face 'italic) ) . 
"Reports an agentset containing all the turtles on the caller's patch (including the caller itself if it's a turtle)" ) 
( "turtles-on" ,(concat "turtles-on " (propertize "agent " 'face 'italic) ) . 
"Reports an agentset containing all the turtles that are on the given patch or patches, or standing on the same patch as the given turtle or turtles" ) 
( "turtles-on" ,(concat "turtles-on " (propertize "agentset " 'face 'italic) ) . 
"Reports an agentset containing all the turtles that are on the given patch or patches, or standing on the same patch as the given turtle or turtles" ) 
( "-on" ,(concat (propertize "<breeds>-on " 'face 'italic) (propertize "agent " 'face 'italic) ) . 
"Reports an agentset containing all the turtles that are on the given patch or patches, or standing on the same patch as the given turtle or turtles" ) 
( "-on" ,(concat (propertize "<breeds>-on " 'face 'italic) (propertize "agentset " 'face 'italic) ) . 
"Reports an agentset containing all the turtles that are on the given patch or patches, or standing on the same patch as the given turtle or turtles" ) 
( "turtles-own" ,(concat "turtles-own " (propertize "[var1 " 'face 'italic) (propertize "...] " 'face 'italic) ) . 
"The turtles-own keyword, like the globals, breed, <breeds>-own, and patches-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "-own" ,(concat (propertize "<breeds>-own " 'face 'italic) (propertize "[var1 " 'face 'italic) (propertize "...] " 'face 'italic) ) . 
"The turtles-own keyword, like the globals, breed, <breeds>-own, and patches-own keywords, can only be used at the beginning of a program, before any function definitions" ) 
( "type" ,(concat "type " (propertize "value " 'face 'italic) ) . 
"Prints value in the Command Center, not followed by a carriage return (unlike print and show)" ) 
( "undirected-link-breed" ,(concat "undirected-link-breed " (propertize "[<link-breeds> " 'face 'italic) (propertize "<link-breed>] " 'face 'italic) ) . 
"This keyword, like the globals and breeds keywords, can only be used at the beginning of the Code tab, before any procedure definitions" ) 
( "untie" ,(concat "untie " ) . 
"Unties end2 from end1 (sets tie-mode to 'none') if they were previously tied together" ) 
( "update-plots" ,(concat "update-plots " ) . 
"For each plot, runs that plot's update commands, including the update code for any pens in the plot" ) 
( "uphill" ,(concat "uphill " (propertize "patch-variable " 'face 'italic) ) . 
"Moves the turtle to the neighboring patch with the highest value for patch-variable" ) 
( "uphill4" ,(concat "uphill4 " (propertize "patch-variable " 'face 'italic) ) . 
"Moves the turtle to the neighboring patch with the highest value for patch-variable" ) 
( "user-directory" ,(concat "user-directory " ) . 
"Opens a dialog that allows the user to choose an existing directory on the system" ) 
( "user-file" ,(concat "user-file " ) . 
"Opens a dialog that allows the user to choose an existing file on the system" ) 
( "user-new-file" ,(concat "user-new-file " ) . 
"Opens a dialog that allows the user to choose a location and name of a new file to be created" ) 
( "user-input" ,(concat "user-input " (propertize "value " 'face 'italic) ) . 
"Reports the string that a user types into an entry field in a dialog with title value" ) 
( "user-message" ,(concat "user-message " (propertize "value " 'face 'italic) ) . 
"Opens a dialog with value displayed as the message" ) 
( "user-one-of" ,(concat "user-one-of " (propertize "value " 'face 'italic) (propertize "list-of-choices " 'face 'italic) ) . 
"Opens a dialog with value displayed as the message and list-of-choices displayed as a popup menu for the user to select from" ) 
( "user-yes-or-no?" ,(concat "user-yes-or-no? " (propertize "value " 'face 'italic) ) . 
"Reports true or false based on the user's response to value" ) 
( "variance" ,(concat "variance " (propertize "list " 'face 'italic) ) . 
"Reports the sample variance of a list of numbers" ) 
( "wait" ,(concat "wait " (propertize "number " 'face 'italic) ) . 
"Wait the given number of seconds" ) 
( "watch" ,(concat "watch " (propertize "agent " 'face 'italic) ) . 
"Puts a spotlight on agent" ) 
( "watch-me" ,(concat "watch-me " ) . 
"Asks the observer to watch this agent" ) 
( "while" ,(concat "while " (propertize "[reporter] " 'face 'italic) (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"If reporter reports false, exit the loop" ) 
( "who" ,(concat "who " ) . 
"This is a built-in turtle variable" ) 
( "with" ,(concat (propertize "agentset " 'face 'italic) "with " (propertize "[reporter] " 'face 'italic) ) . 
"Takes two inputs: on the left, an agentset (usually 'turtles' or 'patches')" ) 
( "<breed>-with" ,(concat "<breed>-with " (propertize "turtle " 'face 'italic) ) . 
"Report the undirected link between turtle and the caller" ) 
( "link-with" ,(concat "link-with " (propertize "turtle " 'face 'italic) ) . 
"Report the undirected link between turtle and the caller" ) 
( "with-max" ,(concat (propertize "agentset " 'face 'italic) "with-max " (propertize "[reporter] " 'face 'italic) ) . 
"Takes two inputs: on the left, an agentset (usually 'turtles' or 'patches')" ) 
( "with-min" ,(concat (propertize "agentset " 'face 'italic) "with-min " (propertize "[reporter] " 'face 'italic) ) . 
"Takes two inputs: on the left, an agentset (usually 'turtles' or 'patches')" ) 
( "with-local-randomness" ,(concat "with-local-randomness " (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"The commands are run without affecting subsequent random events" ) 
( "without-interruption" ,(concat "without-interruption " (propertize "[ " 'face 'italic) (propertize "commands " 'face 'italic) (propertize "] " 'face 'italic) ) . 
"This primitive exists only for backwards compatibility" ) 
( "word" ,(concat "word " (propertize "value1 " 'face 'italic) (propertize "value2 " 'face 'italic) ) . 
"Concatenates the inputs together and reports the result as a string" ) 
( "(word" ,(concat "(word " (propertize "value1 " 'face 'italic) (propertize "...) " 'face 'italic) ) . 
"Concatenates the inputs together and reports the result as a string" ) 
( "world-width" ,(concat "world-width " ) . 
"These reporters give the total width and height of the NetLogo world" ) 
( "world-height" ,(concat "world-height " ) . 
"These reporters give the total width and height of the NetLogo world" ) 
( "wrap-color" ,(concat "wrap-color " (propertize "number " 'face 'italic) ) . 
"wrap-color checks whether number is in the NetLogo color range of 0 to 140 (not including 140 itself)" ) 
( "write" ,(concat "write " (propertize "value " 'face 'italic) ) . 
"This command will output value, which can be a number, string, list, boolean, or nobody to the Command Center, not followed by a carriage return (unlike print and show)" ) 
( "xcor" ,(concat "xcor " ) . 
"This is a built-in turtle variable" ) 
( "xor" ,(concat (propertize "boolean1 " 'face 'italic) "xor " (propertize "boolean2 " 'face 'italic) ) . 
"Reports true if either boolean1 or boolean2 is true, but not when both are true" ) 
( "ycor" ,(concat "ycor " ) . 
"This is a built-in turtle variable" ) 
( "?," ,(concat "?, " (propertize "?1, " 'face 'italic) (propertize "?2, " 'face 'italic) (propertize "?3, " 'face 'italic) (propertize "... " 'face 'italic) ) . 
"These special variable names refer to the inputs to a task, in order by number" ) 
))