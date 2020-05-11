import Main from '../output/Main'

function main() {
    Main.main()
}

if (module.hot) {
    module.hot.accept(() => {
        console.log('Reloaded, running main again')
        main()
    })
}

console.log('Starting App')

main()
