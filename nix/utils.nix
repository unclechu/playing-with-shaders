{}:

{
  getRelativeFileName = prefix: fileName:
    # +1 and -1 for the slash after the prefix
    builtins.substring
      (builtins.stringLength prefix + 1)
      (builtins.stringLength fileName - builtins.stringLength prefix - 1)
      fileName;
}
