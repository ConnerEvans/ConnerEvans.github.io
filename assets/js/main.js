/*
	Phantom by HTML5 UP
	html5up.net | @ajlkn
	Free for personal and commercial use under the CCA 3.0 license (html5up.net/license)
*/

(function($) {

	var	$window = $(window),
		$body = $('body');

	// Breakpoints.
		breakpoints({
			xlarge:   [ '1281px',  '1680px' ],
			large:    [ '981px',   '1280px' ],
			medium:   [ '737px',   '980px'  ],
			small:    [ '481px',   '736px'  ],
			xsmall:   [ '361px',   '480px'  ],
			xxsmall:  [ null,      '360px'  ]
		});

	// Play initial animations on page load.
		$window.on('load', function() {
			window.setTimeout(function() {
				$body.removeClass('is-preload');
			}, 100);
		});

	// Touch?
		if (browser.mobile)
			$body.addClass('is-touch');

	// Forms.
		var $form = $('form');

		// Auto-resizing textareas.
			$form.find('textarea').each(function() {

				var $this = $(this),
					$wrapper = $('<div class="textarea-wrapper"></div>'),
					$submits = $this.find('input[type="submit"]');

				$this
					.wrap($wrapper)
					.attr('rows', 1)
					.css('overflow', 'hidden')
					.css('resize', 'none')
					.on('keydown', function(event) {

						if (event.keyCode == 13
						&&	event.ctrlKey) {

							event.preventDefault();
							event.stopPropagation();

							$(this).blur();

						}

					})
					.on('blur focus', function() {
						$this.val($.trim($this.val()));
					})
					.on('input blur focus --init', function() {

						$wrapper
							.css('height', $this.height());

						$this
							.css('height', 'auto')
							.css('height', $this.prop('scrollHeight') + 'px');

					})
					.on('keyup', function(event) {

						if (event.keyCode == 9)
							$this
								.select();

					})
					.triggerHandler('--init');

				// Fix.
					if (browser.name == 'ie'
					||	browser.mobile)
						$this
							.css('max-height', '10em')
							.css('overflow-y', 'auto');

			});

	// Menu.
		var $menu = $('#menu');

		$menu.wrapInner('<div class="inner"></div>');

		$menu._locked = false;

		$menu._lock = function() {

			if ($menu._locked)
				return false;

			$menu._locked = true;

			window.setTimeout(function() {
				$menu._locked = false;
			}, 350);

			return true;

		};

		$menu._show = function() {

			if ($menu._lock())
				$body.addClass('is-menu-visible');

		};

		$menu._hide = function() {

			if ($menu._lock())
				$body.removeClass('is-menu-visible');

		};

		$menu._toggle = function() {

			if ($menu._lock())
				$body.toggleClass('is-menu-visible');

		};

		$menu
			.appendTo($body)
			.on('click', function(event) {
				event.stopPropagation();
			})
			.on('click', 'a', function(event) {

				var href = $(this).attr('href');

				event.preventDefault();
				event.stopPropagation();

				// Hide.
					$menu._hide();

				// Redirect.
					if (href == '#menu')
						return;

					window.setTimeout(function() {
						window.location.href = href;
					}, 350);

			})
			.append('<a class="close" href="#menu">Close</a>');

		$body
			.on('click', 'a[href="#menu"]', function(event) {

				event.stopPropagation();
				event.preventDefault();

				// Toggle.
					$menu._toggle();

			})
			.on('click', function(event) {

				// Hide.
					$menu._hide();

			})
			.on('keydown', function(event) {

				// Hide on escape.
					if (event.keyCode == 27)
						$menu._hide();

			});

})(jQuery);



/////////////////////////////////////// CLUSTERING ////////////////////////////////////////////

// Structure for cluster files
NAME = 0;
CLUSTER_TYPE = 1;
COMPARISON_ARRAY = 2;
CALCULATED_CLUSTERS = 3;
VISUALIZE_CLUSTERS_FLAG = 4;

// Structure for the comparison array
SIMILARITY = 0;
ITEM_1 = 1;
ITEM_2 = 2;

// Structure for the calculated clusters
INDICATORS = 0;
CLUSTERS = 1;

// Recorded Similarities
NO_INPUT = 0;
SIMILAR = 1;
NOT_SIMILAR = 2;
COMPLETELY_DIFFERENT = 3;


let cluster_files;
if (localStorage.getItem("cluster_files")) {
	cluster_files = JSON.parse(localStorage.getItem("cluster_files"));
} else {
	cluster_files = [];
}

let active_file_idx;
if (localStorage.getItem("active_file_idx")) {
	active_file_idx = JSON.parse(localStorage.getItem("active_file_idx"));
} else {
	active_file_idx = [];
}

window.addEventListener('load', async function () {
	path = window.location.pathname;
	page = path.split("/").pop();

	if (page == "cluster.html") {
		if (cluster_files.length == 0) {
			file_button = document.getElementById("cluster_file0");
			file_button.innerHTML = "create first file";
			file_button.className = "button fit";
			file_button.href = "cluster_file_creation.html";
	
			button_width = document.getElementById("button_width");
			button_width.className = "col-9-xsmall col-9-small col-7-medium col-5";
	
		} else {
			for (let i = 0; i < 5; i++) {
				file_button = document.getElementById("cluster_file"+i.toString());
				delete_button = document.getElementById("delete"+i.toString());
				if (i < cluster_files.length) {
					if (cluster_files[i][NAME] == "") {
						file_button.innerHTML = cluster_files[i][CLUSTER_TYPE];
					} else {
						file_button.innerHTML = cluster_files[i][NAME] + " - " + cluster_files[i][CLUSTER_TYPE];
					}
					
					file_button.className = "button fit";
					file_button.onclick = function () { choose_cluster_file(i); };
					file_button.href = "cluster_inputs.html";
		
					delete_button.className = "button small icon solid fa-trash";
					delete_button.onclick = function () { delete_cluster_file(i); };
					delete_button.href = "cluster.html";
				} else if (i == cluster_files.length) {
					file_button.className = "button large icon solid fa-plus";
					file_button.href = "cluster_file_creation.html";
				} else {
					file_button.innerHTML = null;
					file_button.className = null;
					file_button.onclick = null;
				}
			}
		}
	} else if (page == "cluster_inputs.html"){
		cluster_title = document.getElementById("cluster_title");
		
		if (cluster_files[active_file_idx][NAME] == "") {
			cluster_title.innerHTML = cluster_files[active_file_idx][CLUSTER_TYPE];
		} else {
			cluster_title.innerHTML = cluster_files[active_file_idx][NAME] + " - " + cluster_files[active_file_idx][CLUSTER_TYPE];
		}


		item_1 = document.getElementById("item_1");
		item_2 = document.getElementById("item_2");

		let {index,num_compared} = pick_comparison_index();

		temp = await fetch("./assets/csv/"+cluster_files[active_file_idx][CLUSTER_TYPE]+".csv").then(res=>{return res.text()});
		items = temp.replace(/"/g, '').split('\n');

		item_1.innerHTML = items[cluster_files[active_file_idx][COMPARISON_ARRAY][index][ITEM_1]];
		item_2.innerHTML = items[cluster_files[active_file_idx][COMPARISON_ARRAY][index][ITEM_2]];

		num_comparisons = document.getElementById("num_comparisons");
		len = cluster_files[active_file_idx][COMPARISON_ARRAY].length;
		num_comparisons.innerHTML = "You have made "+num_compared+" comparisons. You will need to make at least about " + parseInt(3.5*(len-20)**.72).toString() + ", but the more you add, the more accurate the clustering will be.";

		if (cluster_files[active_file_idx][CALCULATED_CLUSTERS].length > 0) {
			vis_button = document.getElementById("previous visualization");
			vis_button.innerHTML = "Previous Visualization";
			vis_button.className = "button fit";
			vis_button.onclick = async function () { visualize_previous_clusters(); };
		}
	
	} else if (page == "cluster_visualization.html") {
		/* Storing user's device details in a variable*/
        let details = navigator.userAgent;
  
        /* Creating a regular expression 
        containing some mobile devices keywords 
        to search it in details string*/
        let regexp = /android|iphone|kindle|ipad/i;
  
        /* Using test() method to search regexp in details
        it returns boolean value*/
        let isMobileDevice = regexp.test(details);
  
        if (isMobileDevice) {
			mobile_warning = document.getElementById("mobile warning");
            mobile_warning.innerHTML  = "In order to properly see the clusters, please hold your phone horizontally.<br><br>";
        }

		if (cluster_files[active_file_idx][VISUALIZE_CLUSTERS_FLAG] == 1) {
			visualize_clusters();
		}

		text_input = document.getElementById("data");
		text_input.addEventListener("keyup", async function(event) {
			if (event.key === "Enter") {
				data = text_input.value.trim().split(' ; ');
				active_file_idx = Number(data[0]);
				indicators = data[1].slice(1,data[1].length-1).split(', ').map(i=>Number(i));

				clusters_string = data[2].slice(1,data[2].length-1).split('], [');
				clusters_string[0] = clusters_string[0].slice(1,clusters_string[0].length)
				last_idx = clusters_string.length - 1;
				clusters_string[last_idx] = clusters_string[last_idx].slice(0,clusters_string[last_idx].length-1);
				clusters = clusters_string.map(list => list.split(', ').map(i=>Number(i)))
				
				cluster_files[active_file_idx][CALCULATED_CLUSTERS] = [];
				cluster_files[active_file_idx][CALCULATED_CLUSTERS].push(indicators);
				cluster_files[active_file_idx][CALCULATED_CLUSTERS].push(clusters);

				localStorage.setItem('cluster_files', JSON.stringify(cluster_files));

				visualize_clusters();
			}
		});
	}
})


async function visualize_clusters(){
	indicators = cluster_files[active_file_idx][CALCULATED_CLUSTERS][INDICATORS];
	clusters  = cluster_files[active_file_idx][CALCULATED_CLUSTERS][CLUSTERS];

	num_clusters = indicators.length - 1;
	first_col = 6 - Math.floor(num_clusters/2);

	temp = await fetch("./assets/csv/"+cluster_files[active_file_idx][CLUSTER_TYPE]+".csv").then(res=>{return res.text()});
	items = temp.replace(/"/g, '').split('\n');

	if (indicators[0]==1) {
		col = document.getElementById("col"+(first_col + num_clusters + 1).toString());
		col_string = "";
		for (let item_i = 0; item_i < clusters[cluster_i].length; item_i++) {
			col_string = col_string + items[clusters[0][item_i]].toUpperCase() + "<br>";
		}
		col.innerHTML = col_string;

		first_col = first_col - 2
	}

	for (let cluster_i = 1; cluster_i < num_clusters + 1; cluster_i++) {
		col = document.getElementById("col"+(first_col + cluster_i).toString());
		col_string = "";
		for (let item_i = 0; item_i < clusters[cluster_i].length; item_i++) {
			item = items[clusters[cluster_i][item_i]]
			if (clusters[cluster_i][item_i] == indicators[cluster_i]) {
				item = item.toUpperCase()
			}
			col_string = col_string + item + "<br>";	
		}
		col.innerHTML = col_string;
	}


	data_input = document.getElementById("data input");
	data_input.remove();

	explanation = document.getElementById("explanation");
	explanation.innerHTML = "This is the clustering that emerged given your input.<br>" +
	"The clusters are ordered from left to right so that the clusters most different from each other are furthest apart. " +
	"Similarly the items within each cluster are orderd from top to bottom so that the items that are the most different are the furthest apart. " +
	"For clusters with three or more items, the item that most exemplifies the cluster is capitalized.";


	cluster_files[active_file_idx][VISUALIZE_CLUSTERS_FLAG] = 0;
	localStorage.setItem('cluster_files', JSON.stringify(cluster_files));
}


function visualize_previous_clusters(){
	cluster_files[active_file_idx][VISUALIZE_CLUSTERS_FLAG] = 1;
	localStorage.setItem('cluster_files', JSON.stringify(cluster_files));

	window.open("cluster_visualization.html", "_self");
}


function add_new_cluster(type){
	file = [];
	file_name = document.getElementById("file_name").value;
	cluster_type = type;
	switch(type) {
		case 'Sports':
			num_elements = 10;
			break;
		case 'NFL Teams':
			num_elements = 32;
			break;
		case 'Twenty One Pilots Songs':
			num_elements = 81;
			break;
		case 'Taylor Swift Songs':
			num_elements = 172;
			break;
	}

	comparison_array = Array();
	for(var i = 1; i < num_elements; i++) {
		for(var j = 0; j < i; j++) {
			comparison_array.push([NO_INPUT,j,i]);
		}
	}

	calculated_clusters = [];

	file.push(file_name);
	file.push(cluster_type);
	file.push(comparison_array);
	file.push(calculated_clusters);
	file.push(0); // set VISUALIZE_CLUSTERS_FLAG to 0

	cluster_files.push(file);
	localStorage.setItem('cluster_files', JSON.stringify(cluster_files));
}

function start_edit_file_name(){
	cluster_title = document.getElementById("cluster_title");
	cluster_title.style.display = "none";

	cluster_title_container = document.getElementById("cluster_title_container");
	cluster_title_container.className = "button col-10-xsmall col-8-small col-6-medium col-4"
	file_name_input = document.createElement("INPUT");
	cluster_title_container.appendChild(file_name_input);

	file_name_input.setAttribute("type", "text");
	file_name_input.setAttribute("name", "file_name");
	file_name_input.setAttribute("id", "file_name_input");
	file_name_input.setAttribute("placeholder", cluster_files[active_file_idx][NAME]);

	edit_button = document.getElementById("edit_button");
	edit_button.className = "button large icon solid fa-check";
	edit_button.onclick = function () { end_edit_file_name(); };
}

function end_edit_file_name(){
	cluster_files[active_file_idx][NAME] = document.getElementById("file_name_input").value;
	localStorage.setItem('cluster_files', JSON.stringify(cluster_files));
	location.reload();
}

function choose_cluster_file(file_idx){
	localStorage.setItem('active_file_idx', JSON.stringify(file_idx));
}

function delete_cluster_file(file_idx){
	cluster_files.splice(file_idx,1);
	localStorage.setItem('cluster_files', JSON.stringify(cluster_files));
}

function pick_comparison_index(){
	len = cluster_files[active_file_idx][COMPARISON_ARRAY].length;
	potential_choice_indexes = Array();

	for(var i = 0; i < len; i++) {
		if (cluster_files[active_file_idx][COMPARISON_ARRAY][i][SIMILARITY] == NO_INPUT) {
			potential_choice_indexes.push(i);
		}
	}

	index = potential_choice_indexes[Math.floor(Math.random() * potential_choice_indexes.length)];
	num_compared = len - potential_choice_indexes.length;
	
	return {index,num_compared};
}

function input_similarity(similarity){
	cluster_files[active_file_idx][COMPARISON_ARRAY][index][SIMILARITY] =  parseInt(similarity);
	localStorage.setItem('cluster_files', JSON.stringify(cluster_files));
	location.reload();
}

function copy_data(){
	data = Array();
	data.push(active_file_idx);

	for(var i = 0; i < cluster_files[active_file_idx][COMPARISON_ARRAY].length; i++) {
		data.push(cluster_files[active_file_idx][COMPARISON_ARRAY][i][SIMILARITY]);
	}

	navigator.clipboard.writeText(data);
}



/////////////////////////////////////// EXPECTED GROWTH ////////////////////////////////////////////