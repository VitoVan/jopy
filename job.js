//初始化地图对象，加载地图
var map = new AMap.Map("mapContainer", {
    resizeEnable: true,
    features: ['point','road','building'],
    zoom: 12 //地图显示的缩放级别
});

AMap.event.addListener(map, 'complete', function(result) {
    map.getCity(function(city){
        city_name = city.city.substr(0,2)
        if(city_name != '北京' && city_name != '上海' && city_name != '广州' && city_name != '郑州'){
            alert('当前仅支持 北京、上海、广州、郑州， ' + city_name + ' 暂未开通。');
            map.setCity('北京')
        }
    })
});
var cloudDataLayer = false;
var keywords = '';
var min_salary = 0;
var max_salary = 99999;
var work_year = 99;
var hours = 48;

function setPlaceholder(id, value){
    if(value !== ''){
        document.getElementById(id).placeholder = value;
    }
}

function getValue(id){
    return document.getElementById(id).value;
}

function setAllPlaceholder(){
    setPlaceholder('min_salary', min_salary);
    setPlaceholder('max_salary', max_salary);
    setPlaceholder('work_year', work_year);
}

function inputKeydown(e){
    if(event.keyCode == 13) {
        searchData();
    }
}

function searchData(){
    keywords = getValue('keywords');
    min_salary = getValue('min_salary');
    max_salary = getValue('max_salary');
    work_year = getValue('work_year');
    addCloudLayer();
}

//叠加云数据图层
function addCloudLayer() {
    setAllPlaceholder();
    if(cloudDataLayer !== false){
        map.clearInfoWindow();
        cloudDataLayer.setMap(null);
    }
    //加载云图层插件
    map.plugin('AMap.CloudDataLayer', function() {
        var timeStamp = Date.now()/1000;
        var layerOptions = {
            query: {
                keywords: keywords,
                filter: 'timestamp:[' + (timeStamp - 60*60*hours) + ',' + timeStamp + ']' +
                    '+min_salary:[' + min_salary + ',99999]+' +
                    'max_salary:[0,' + max_salary + ']+' +
                    'min_work_year:[0,' + work_year + ']'
            },
            clickable: true
        };
        cloudDataLayer = new AMap.CloudDataLayer('56c526a5305a2a32880a1fed', layerOptions); //实例化云图层类
        cloudDataLayer.setMap(map); //叠加云图层到地图
        
        AMap.event.addListener(cloudDataLayer, 'click', function(result) {
            var clouddata = result.data;
            var title = clouddata._name;
            var content = [];
            content.push("地址：" + clouddata._address + "<br />" + "创建时间：" + clouddata._createtime + "<br />" + "更新时间：" + clouddata._updatetime);
            var infoWindow = new AMap.InfoWindow({
                content: '<div class="job-info">' + '<div class="job-title"><a href="' + clouddata.raw_url + '" target="_blank">' + title + '</a></div>' +
                    '<div>' + clouddata.description.substring(0,200).trim() + 
                    '...    <a href="' + clouddata.raw_url + '" target="_blank">More</a></div>' +
                    '<div>' + ((clouddata.min_salary == 0 && clouddata.max_salary == 0) ? '薪资不明' : ('月薪：' + clouddata.min_salary + ' - ' + clouddata.max_salary + ' RMB')) + '</div>' +
                    '<div>' + ((clouddata.min_work_year == 0 && clouddata.max_work_year == 0) ? '无经验要求' : ('<div>经验：' + clouddata.min_work_year + ' - ' + clouddata.max_work_year + ' 年')) + '</div>' +
                    '<div class="job-company">' + clouddata.company + '</div>' +
                    '</div>',
                size: new AMap.Size(0, 0),
                autoMove: true,
                offset: new AMap.Pixel(0, -25)
            });
            infoWindow.open(map, clouddata._location);
        });
    });
}
addCloudLayer();
