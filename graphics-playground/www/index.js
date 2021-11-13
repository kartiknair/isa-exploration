import * as wasm from 'isas'
import { editor } from 'monaco-editor'

const sourceEditor = editor.create(document.getElementById('source-editor'), {
	minimap: { enabled: false },
	scrollbar: { vertical: 'hidden' },
	lineNumbers: 'off',
	glyphMargin: false,
	fontLigatures: true,
	automaticLayout: true,
	fontFamily: 'JetBrains Mono',
	fontSize: '13px',
	lineHeight: '1.5',
	theme: 'vs-dark',
	language: 'rust',
})

const canvas = document.createElement('display')
const ctx = canvas.getContext('2d')

const update = () => {
	let source = sourceEditor.getValue()

	const palette = ctx.getImageData(0, 0, 640, 480)
	palette.data.set(new Uint8ClampedArray(img)) // assuming values 0..255, RGBA, pre-mult.
	// Repost the data.
	ctx.putImageData(palette, 0, 0)
}

update()
sourceEditor.getModel().onDidChangeContent(update)
