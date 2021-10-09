import * as wasm from 'isas'
import { editor } from 'monaco-editor'

let commonSettings = {
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
}

let sourceEditor = editor.create(document.getElementById('source-editor'), {
	value: 'fun main() {\n  var n = 45\n  print_i32(n)\n}\n',
	language: 'plain',
	...commonSettings,
})

let riscAsmEditor = editor.create(document.getElementById('risc-asm'), {
	value: '',
	language: 'plain',
	readOnly: true,
	...commonSettings,
})

let ciscAsmEditor = editor.create(document.getElementById('cisc-asm'), {
	value: '',
	language: 'plain',
	readOnly: true,
	...commonSettings,
})

let riscOutputEditor = editor.create(document.getElementById('risc-terminal'), {
	value: '',
	language: 'plain',
	readOnly: true,
	...commonSettings,
})

let ciscOutputEditor = editor.create(document.getElementById('cisc-terminal'), {
	value: '',
	language: 'plain',
	readOnly: true,
	...commonSettings,
})

const updateEditors = () => {
	let contents = sourceEditor.getValue()

	let riscAsm = wasm.compile_risc(contents)
	let ciscAsm = wasm.compile_cisc(contents)

	let riscStdio = wasm.run_risc(contents)
	let ciscStdio = wasm.run_cisc(contents)

	console.log(riscAsm)
	console.log(ciscAsm)

	console.log(riscStdio)
	console.log(ciscStdio)

	riscAsmEditor.getModel().setValue(riscAsm || '; error')
	ciscAsmEditor.getModel().setValue(ciscAsm || '; error')

	riscOutputEditor.getModel().setValue(riscStdio)
	ciscOutputEditor.getModel().setValue(ciscStdio)
}

updateEditors()
sourceEditor.getModel().onDidChangeContent(updateEditors)
