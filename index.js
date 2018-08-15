/* global Elm, JSZip, saveAs, prettier, prettierPlugins */
const app = Elm.Main.fullscreen()

app.ports.zip.subscribe(zip)

function zip ([name, files]) {
  const zip = new JSZip()
  files.forEach(file => addFile(zip, file))
  zip.generateAsync({ type: 'blob' }).then(blob => saveAs(blob, name))
}

function addFile (zip, file) {
  if (file.type === 'directory') {
    const directory = zip.folder(file.name)
    file.files.forEach(currFile => addFile(directory, currFile))
  } else {
    zip.file(file.name, format(file.name, file.content))
  }
}

function format (name, content) {
  const ext = name.split('.').reverse()[0]
  const config = { parser: 'babylon', plugins: prettierPlugins }
  return ['js', 'json'].includes(ext) ? prettier.format(content, config) : content
}
