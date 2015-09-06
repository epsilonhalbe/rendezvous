$(document).ready(function(){
"use strict";

  $(".btn-warning").click(function(){
    $(this).parent().children(".form-control").css("display", "inline");
    $(this).parent().children(".btn").css("padding-left", "6px");
    $(this).parent().children(".btn").css("padding-right", "6px");
    $(this).parent().children(".form-control").focus();
  });
  $(".form-control").blur(function(){
    $(this).css("display", "none");
    $(this).parent().children(".btn").css("padding-left", "16px");
    $(this).parent().children(".btn").css("padding-right", "16px");
  });
});
