const gulp = require("gulp"),
  rename = require("gulp-rename"),
  uglify = require("gulp-uglify"),
  gls = require("gulp-live-server"),
  webpackStream = require("webpack-stream"),
  sass = require("gulp-sass"),
  browserSync = require("browser-sync").create(),
  useref = require("gulp-useref"),
  pug = require("gulp-pug"),
  combine = require("gulp-scss-combine"),
  sassGlob = require("gulp-sass-glob"),
  concat = require("gulp-concat");
// gulpIf = require("gulp-if");

gulp.task("scripts", function () {
  return (
    gulp
      .src("./src/js/**/*.js")
      .pipe(
        webpackStream({
          output: {
            filename: "app.js",
          },
          module: {
            rules: [
              {
                test: /\.(js)$/,
                exclude: /(node_modules)/,
                loader: "babel-loader",
              },
            ],
          },
        })
      )
      .pipe(gulp.dest("./dist"))
      // .pipe(uglify())
      .pipe(rename({ suffix: ".min" }))
      .pipe(gulp.dest("./dist"))
      .pipe(
        browserSync.reload({
          stream: true,
        })
      )
  );
});

gulp.task("sass", function () {
  return gulp
    .src("src/scss/**/*.scss")
    .pipe(sassGlob())
    .pipe(sass())
    .pipe(gulp.dest("dist/css"))
    .pipe(
      browserSync.reload({
        stream: true,
      })
    );
});

gulp.task("browserSync", function () {
  browserSync.init({
    server: {
      baseDir: "dist",
    },
  });
});

gulp.task("pug", function () {
  return gulp
    .src("src/pug/**/*.pug")
    .pipe(pug())
    .pipe(gulp.dest("./dist/"))
    .pipe(
      browserSync.reload({
        stream: true,
      })
    );
});

gulp.task(
  "watch",
  gulp.parallel([
    "browserSync",
    "pug",
    "sass",
    "scripts",
    function () {
      gulp.watch("src/scss/**/*.scss", gulp.series(["sass"]));
      gulp.watch("src/js/**/*.js", gulp.series(["scripts"]));
      gulp.watch("src/pug/**/*.pug", gulp.series(["pug"]));
      return;
      //     gulp.watch("src/scss/**/*.scss", gulp.parallel("sass"));
      // gulp.watch("src/js/**/*.js", gulp.parallel("scripts"));
      // gulp.watch("src/pug/**/*.pug", gulp.parallel("pug"));
    },
  ])
);
