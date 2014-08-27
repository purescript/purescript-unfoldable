module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs"
    ],
    
    clean: ["output"],
  
    pscMake: {
      lib: {
        src: ["<%=libFiles%>"]
      }
    },

    psc: {
      options: {
        modules: ["Main"],
        main: "Main"
      },
      example: {
        src: ["<%=libFiles%>", "examples/*.purs"],
        dest: "dist/Main.js"  
      }
    },

    dotPsci: ["<%=libFiles%>"]
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
 
  grunt.registerTask("make", ["pscMake:lib", "dotPsci", "psc:example"]);
  grunt.registerTask("default", ["clean", "make"]);
};
