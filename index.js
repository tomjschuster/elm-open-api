/* global Elm, JSZip, saveAs, prettier, prettierPlugins */
const app = Elm.Main.fullscreen()
app.ports.zip.subscribe(zip)

function zip ([name, files]) {
  const zip = new JSZip()
  files.forEach(file => zipFile(zip, file))
  zip.generateAsync({ type: 'blob' }).then(blob => saveAs(blob, name))
}

function zipFile (zip, file) {
  if (file.type === 'directory') zipDirectory(zip, file)
  else zipNormalFile(zip, file)
}

function zipNormalFile (zip, file) {
  zip.file(file.name, format(file.name, file.content))
}

function zipDirectory (zip, directory) {
  const folder = zip.folder(directory.name)
  directory.files.forEach(currFile => zipFile(folder, currFile))
}

function format (name, content) {
  const ext = name.split('.').reverse()[0]
  const config = { parser: 'babylon', plugins: prettierPlugins }
  return ['js', 'json'].includes(ext) ? prettier.format(content, config) : content
}
