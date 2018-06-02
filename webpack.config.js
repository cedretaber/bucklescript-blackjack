const path = require("path");

module.exports = {
    mode: "development",
    module: {
        rules: [
            {
                test: /\.css$/,
                loaders: ["style-loader", "css-loader"]
            }
        ]
    },
    entry: "./src/index.bs.js",
    output: {
        filename: "bundle.js",
        path: path.resolve(__dirname, "dist")
    }
}