module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs"
    ],

    clean: ["output", "tmp"],

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    pscDocs: {
        readme: {
            src: "src/**/*.purs",
            dest: "README.md"
        }
    },

    psc: {
      collatzExample: {
        options: {
          modules: ["Main"],
          main: "Main"
        },
        src: ["<%=libFiles%>", "examples/Collatz.purs"],
        dest: "tmp/Collatz.js"
      }
    },

    execute: {
      collatzExample: {
        src: "tmp/Collatz.js"
      }
    },

    jsvalidate: ["output/**/*.js"]

  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-execute");
  grunt.loadNpmTasks("grunt-jsvalidate");
  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("make", ["pscMake", "dotPsci", "pscDocs", "jsvalidate"]);
  grunt.registerTask("examples", ["psc", "execute"]);
  grunt.registerTask("default", ["clean", "make"]);
};
