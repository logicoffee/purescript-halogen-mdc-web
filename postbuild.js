const fs = require('fs')
const path = require('path')

if (!fs.existsSync(path.join(__dirname, 'dist', 'public'))){
  fs.mkdirSync(path.join(__dirname, 'dist', 'public'));
}

fs.readdir('./examples/images', (_, images) => {
  images.forEach(image => {
    const from = path.join('./examples/images', image)
    const to = path.join('./dist/public', image)
    fs.rename(from, to, err => {
      if (err) return console.log(err)
      console.log('place images to public folder')
    })
  })
})

const matchImage = file => {
  const jpeg = /.+\.jpeg$/i
  const jpg = /.+\.jpg$/i
  const png = /.+\.png$/i
  const gif = /.+\.gif$/i
  return file.match(jpeg) || file.match(jpg) || file.match(png) || file.match(gif)
}
