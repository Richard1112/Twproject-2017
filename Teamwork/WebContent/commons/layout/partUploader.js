  function uplClickManage(el){
    //console.debug("uplClickManage");
    var table=el.closest("table.upvtable");
    var spanFi=table.find(".sp_fi");
    var spanTf=table.find(".sp_tf");
    var img=table.find("img");
    var upl=table.find("[type=file]");

    if (!spanFi.is(":visible")){ //click on X
      spanFi.show();
      upl.get(0).disabled=false;
      upl.data("_oldvalue","sporc"); // hack as the val() for file in unchangeable
      spanTf.hide();
      img.prop("src",contextPath + "/img/uploader/link.png");
    } else { //click on rondell
      spanFi.hide();
      upl.get(0).disabled=true;
      upl.updateOldValue();
      spanTf.show();
      img.prop("src",contextPath + "/img/uploader/unlink.png");

    }
  }
