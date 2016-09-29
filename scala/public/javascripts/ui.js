$(document).ready(function(){
    $('#orderOps a').click(function (e) {
      e.preventDefault();
      var target = $(e.target).attr("href");
      log(target);
      $(this).tab('show');
    });
});

