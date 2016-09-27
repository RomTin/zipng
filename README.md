# This is a zip. This is a png

**!! UNFORTUNATELY, sometimes archive manager CAN'T extract generated zip archive file. please use `unzip` command to extract that !!**

This is a command line tool to generate png file which hides a zip file in its zTXT chunk.

ZIP will be hidden in PNG as zTXT chunk, and that PNG is still surely **VALID**.

On the other hand, PNG will be hidden in ZIP as extra data that is located in forehand section of that ZIP, and that ZIP is also still surely **VALID**.

### dependencies
* Erlang/OTP > 18

### run
```
$ chmod u+x ./*.erl
$ ./zipng.erl path/to/png path/to/file1 path/to/file2 ...
%% file1,2...will be zipped and appended to png
```

### notifications
* Function Unused WARNINGS will be appeared when you run erlang script. Please ignore them.
* Huge file payload may cause unexpected error. Contents of ZIP file should be within 4.0MB.