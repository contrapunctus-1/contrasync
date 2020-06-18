# Certain
1. [ ] Multi-machine use - user specifies a machine name; the destination path finally becomes `<destination path>/<machine name>/<source path>/`
2. Progress bar
3. Rename "destination" to "prefix", to clarify its use; I want the user to only have to specify it once per backup device.

# Maybe
1. Better default value for `rsync-max-procs`? Some way to determine the ideal value, maybe using number of CPU cores and/or network bandwidth?
