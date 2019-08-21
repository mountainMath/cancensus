og_img1='<meta property="og:image" content="/logo.png"'
og_img2='<meta property="og:image" content="https://mountainmath.github.io/cancensus/logo.png"'

replace_meta () {
  file=$(find . -name '*.html')
  for i in $file; do
    cat "${i}" | sed "s,${og_img1},${og_img2},g" > "temp"
    cat "temp" > "${i}"
  done
  rm temp
}

cd ./docs
  replace_meta
  cd articles
  replace_meta
  cd ..
  cd news
  replace_meta
  cd ..
  cd reference
  replace_meta
  cd ..
cd ..
