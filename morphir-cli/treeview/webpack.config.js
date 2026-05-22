const path = require('path');
const { webpack } = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: path.resolve(__dirname, './src/index.ts'), 

  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'dist'),
  },
  
  target: 'node', 
  mode: 'none',
  resolve: {
    extensions: [ '.js' ,'.ts']
  },

  module: {
    rules: [
      {
        test: /\.tsx?$/,
        include: path.resolve(__dirname, "./src/"),
        exclude: /node_modules/,
        loader: "ts-loader",
      },
      {
        // morphir-ts/dist files use ESM syntax (`export *`) even though the
        // package root is "type": "commonjs". Tell webpack to parse them as
        // ESM directly. The fullySpecified: false bit is needed because the
        // morphir-generated files omit explicit .js extensions on imports.
        test: /\.js$/,
        include: path.resolve(__dirname, "../../morphir-ts"),
        type: "javascript/esm",
        resolve: {
          fullySpecified: false,
        },
      }
    ]
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: path.resolve(__dirname, './src/index.html'),
      filename: 'index.html'
    })
  ],

  devtool: 'inline-source-map',
};
