$(document).on("click", ".dynamic-tag", function () {
    jQuery
	.ajax({
	    url: $(this).data("dynamic-url"),
	    dataType: "json"
	})
	.done(function (result) {
	    switch (result.operation) {
	    case "add":
		$(result.selector).append(result.value);
		break;
	    case "replace":
		$(result.selector).replaceWith(result.value);
		break;
	    case "remove":
		$(result.selector).remove();
		break;
	    case "nothing":
		break;
	    default:
		throw "Unknown operation: " + result.operation;
	    }
	});
});
