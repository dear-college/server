const path = require("path");
const webpack = require('webpack');
const fs = require('fs');

module.exports = {
  mode: 'development',
  // mode: 'production',
  entry: {
    main: './src/index.js'
  },
  output: {
    path: path.join(__dirname, 'public'),
    publicPath: '/',
    filename: 'dear-college.js',
    library: {
      type: "umd",
      name: "client",
    },
  },
  target: 'web',
  devtool: 'source-map',
  module: {
    rules: [
      {
        test: /\.js$/,
        loader: "babel-loader",
      },
    ]
  }
};
