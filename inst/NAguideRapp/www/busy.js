setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    setTimeout(function() {
      if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show()
      }
    }, 1000)
  } else {
    $('div.busy').hide()
  }
}, 100)

$(function () {
			var x = 10;
			var y = 20;
			$("#span1").mouseover(function (e) {
				this.myTitle = this.title;
				this.title = "";
				var tooltip = "<div id='tooltip'>" + this.myTitle + "<\/div>"; //创建 div 元素 文字提示
				$("body").append(tooltip);	//把它追加到文档中
				$("#tooltip").css({
					"top": (e.pageY + y) + "px",
					"left": (e.pageX + x) + "px"
				}).show("fast");	  //设置x坐标和y坐标，并且显示
			}).mouseout(function () {
				this.title = this.myTitle;
				$("#tooltip").remove();   //移除 
			}).mousemove(function (e) {
				$("#tooltip").css({
					"top": (e.pageY + y) + "px",
					"left": (e.pageX + x) + "px"
				});
			});
		})

$(function () {
			var x = 10;
			var y = 20;
			$("#span2").mouseover(function (e) {
				this.myTitle = this.title;
				this.title = "";
				var tooltip = "<div id='tooltip2'>" + this.myTitle + "<\/div>"; //创建 div 元素 文字提示
				$("body").append(tooltip);	//把它追加到文档中
				$("#tooltip2").css({
					"top": (e.pageY + y) + "px",
					"left": (e.pageX + x) + "px"
				}).show("fast");	  //设置x坐标和y坐标，并且显示
			}).mouseout(function () {
				this.title = this.myTitle;
				$("#tooltip2").remove();   //移除 
			}).mousemove(function (e) {
				$("#tooltip2").css({
					"top": (e.pageY + y) + "px",
					"left": (e.pageX + x) + "px"
				});
			});
		})

$(function () {
			var x = 10;
			var y = 20;
			$("#span3").mouseover(function (e) {
				this.myTitle = this.title;
				this.title = "";
				var tooltip = "<div id='tooltip3'>" + this.myTitle + "<\/div>"; //创建 div 元素 文字提示
				$("body").append(tooltip);	//把它追加到文档中
				$("#tooltip3").css({
					"top": (e.pageY + y) + "px",
					"left": (e.pageX + x) + "px"
				}).show("fast");	  //设置x坐标和y坐标，并且显示
			}).mouseout(function () {
				this.title = this.myTitle;
				$("#tooltip3").remove();   //移除 
			}).mousemove(function (e) {
				$("#tooltip3").css({
					"top": (e.pageY + y) + "px",
					"left": (e.pageX + x) + "px"
				});
			});
		})

$(function () {
			var x = 10;
			var y = 20;
			$("#span4").mouseover(function (e) {
				this.myTitle = this.title;
				this.title = "";
				var tooltip = "<div id='tooltip4'>" + this.myTitle + "<\/div>"; //创建 div 元素 文字提示
				$("body").append(tooltip);	//把它追加到文档中
				$("#tooltip4").css({
					"top": (e.pageY + y) + "px",
					"left": (e.pageX + x) + "px"
				}).show("fast");	  //设置x坐标和y坐标，并且显示
			}).mouseout(function () {
				this.title = this.myTitle;
				$("#tooltip4").remove();   //移除 
			}).mousemove(function (e) {
				$("#tooltip4").css({
					"top": (e.pageY + y) + "px",
					"left": (e.pageX + x) + "px"
				});
			});
		})

$(function () {
			var x = 10;
			var y = 20;
			$("#spanx5").mouseover(function (e) {
				this.myTitle = this.title;
				this.title = "";
				var tooltip = "<div id='tooltipx5'>" + this.myTitle + "<\/div>"; //创建 div 元素 文字提示
				$("body").append(tooltip);	//把它追加到文档中
				$("#tooltipx5").css({
					"top": (e.pageY + y) + "px",
					"left": (e.pageX + x) + "px"
				}).show("fast");	  //设置x坐标和y坐标，并且显示
			}).mouseout(function () {
				this.title = this.myTitle;
				$("#tooltipx5").remove();   //移除 
			}).mousemove(function (e) {
				$("#tooltipx5").css({
					"top": (e.pageY + y) + "px",
					"left": (e.pageX + x) + "px"
				});
			});
		})