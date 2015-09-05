$(document).ready(function(){
"use strict";

  $(".btn-warning").click(function(){
    $(this).parent().children(".form-control").css("display", "inline");
    $(this).parent().children(".btn").css("padding", "6px 4px 6px 4px");
    $(this).parent().children(".form-control").focus();
  });
  $(".form-control").blur(function(){
    $(this).css("display", "none");
    $(this).parent().children(".btn").css("padding", "6px 12px 6px 12px");
  });
});
