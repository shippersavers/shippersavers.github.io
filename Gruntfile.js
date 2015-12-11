module.exports = function(grunt) {

  grunt.initConfig({
    elm: {
      compile: {
        files: {
          "main.js": ["Main.elm"]
        }
      }
    },
    sass: {
      options: {
        sourceMap: true
      },
      dist: {
        files: {
          'style.css': 'style.scss'
        }
      }
    },
    watch: {
      elm: {
        files: ["Main.elm", "Seaport.elm", "SeaportPair.elm", "Tariff.elm", "style.scss"],
        tasks: ["elm", "sass"]
      }
    },
    clean: ["elm-stuff/build-artifacts"]
  });

  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-elm');
  grunt.loadNpmTasks('grunt-sass');

  grunt.registerTask('default', ['elm']);
  grunt.registerTask('default', ['sass']);

};
