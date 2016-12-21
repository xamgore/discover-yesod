ymaps.ready(init);

function addMark(map, position, photoUrl, placeUrl) {
        mark = new ymaps.Placemark(position, {
            balloonContent: photoUrl
        }, {
            iconLayout: 'default#image',
            iconImageHref: 'images/sprite.png',
            iconImageSize: [92, 92],
            iconImageOffset: [-46, -32]
        });

    map.geoObjects.add(mark);
}

function init() {

    var map = new ymaps.Map("map", {
            center: [47.217268, 39.716645],
            zoom: 15,
            controls: []
        }, {
            searchControlProvider: 'yandex#search'
        });

    addMark(map, [47.217268, 39.7166410], "asasasa");

}
