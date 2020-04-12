Evernote doesn't export the notebook and stack structure of your notes. To give the same sort of
vibe, I just export each notebook individually. My suggestion is to create a directory structure
where you have notebooks as `.enex` files and stacks as directories. Something like:

```
exports-root
  notebook0.enex
  notebook1.enex
  stack1
    notebook2.enex
    notebook3.enex
  stack2
    notebook4.enex
    notebook5.enex
```

If you have too many notebooks/stacks to do this easily, you're kind of on your own.

Once you've got a structure like that, run the following (where `<dest>` is the directory where
you want to create the corresponsing markdown file hierarchy):

```bash
sbt 'runMain convert.StructureConverter <exports-root> <dest>'
```