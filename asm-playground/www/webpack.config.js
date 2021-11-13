const CopyWebpackPlugin = require('copy-webpack-plugin')
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin')
const path = require('path')

module.exports = {
	entry: './bootstrap.js',
	output: {
		path: path.resolve(__dirname, 'public'),
		filename: 'bootstrap.js',
	},
	module: {
		rules: [
			{
				test: /\.css$/,
				use: ['style-loader', 'css-loader'],
			},
			{
				test: /\.ttf$/,
				use: ['file-loader'],
			},
		],
	},
	experiments: {
		asyncWebAssembly: true,
	},
	mode: 'development',
	plugins: [new MonacoWebpackPlugin(), new CopyWebpackPlugin(['index.html'])],
}
