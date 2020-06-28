# Serialization
---

### Related packages:
 * `pcg`

### Related modules:

 - **`PCG.Command.Load`**
 - **`PCG.Command.Report.Distance`**
 - **`PCG.Command.Report.Evaluate`**
 - **`PCG.Command.Report.GraphViz`**
 - **`PCG.Command.Report.ImpliedAlignment`**
 - **`PCG.Command.Report.Metadata`**
 - **`PCG.Command.Save`**

There are two forms of serialization that PCG uses. The first is binary "save state" serialization via the `Load` and `Save` commands. The second is file streaming, supporting a variety of data formats.

The `Load` and `Save` commands are used to create checkpoints for PCG. The `Load` command will read a save state from the disk and overwrite the current graph state in memory with the data read from the save state. The `Save` command takes the current graph state in memory and write it out to disk for later usage. 

PCG can also stream output to files. File streaming is initiated via the `Report` command, which specifies which information and file formats are streamed to disk. Formats can be written to disk as lazy text or as bytestrings. Some report command options support multi-file streaming in parallel. All formats take the current working graph state of PCG and use this as input for the file streaming. All report commands support three different writing modes overwrite, append, and move:

 - **`overwrite`:** If the file already exists, it is overwritten with the new file stream
 - **`append`:** If the file already exists, the new file stream is placed at the end of the existing file
 - **`move`:** If the file already exists, the old file is rename (moved), and the new file stream is written to the old files name.

The `Report` command is designed to be extensible, making it easy to add additional reporting formats. Consequently, the `Report` command already has several formats. The `data` format is a "human readable" rendering of the entire graph state. This format is recommend for debugging purposes. Other formats include XML and DOT.
