/* global Elm, JSZip, saveAs, prettier, prettierPlugins */
const app = Elm.Main.fullscreen()

app.ports.zip.subscribe(([name, files]) => {
  const zip = new JSZip()
  files.forEach(file => addFile(zip, file))
  zip.generateAsync({ type: 'blob' }).then(blob => saveAs(blob, name))
})

const addFile = (zip, file)  => {
  if (file.type === 'directory') {
    const directory = zip.folder(file.name)
    file.files.forEach(currFile => addFile(directory, currFile))
  } else if (file.type === 'file') {
    zip.file(file.name, format(file.name, file.content))
  } else {
    throw ('Invalid File Type ' + file.type)
  }
}

const format = (name, content) => usePrettier(name) ? formatPrettier(content) : content
const usePrettier = name => (['js', 'jsx', 'json'].includes(ext(name)))
const ext = fileName => fileName.split('.').reverse()[0]
const prettierConfig = { parser: 'babylon', plugins: prettierPlugins }
const formatPrettier = content => prettier.format(content, prettierConfig)