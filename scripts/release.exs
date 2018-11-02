IO.puts "Starting ... \n"

release_dir = "dist/"

if File.exists?(release_dir) do
	IO.puts "Removing old release: #{release_dir} directory ..."

	case File.rm_rf(release_dir) do
		{:ok, _files} ->
			IO.puts "Done.\n"
		{:error, error, files} ->
			IO.puts "  Error: #{error} #{files}"
	end
end

# ------------------------------------

IO.puts "Building ..."

System.cmd("parcel", ["build", "index.html", "-o", "baseof.html"], 
									into: IO.stream(:stdio, :line))
IO.puts "Done.\n"

# ------------------------------------
	
IO.puts "Removing old files from Hugo site ..."

base_hugo_site = "../../Hugo/streaming-spring/"
ocaml_layouts = "layouts/ocaml/"
static_dir = "static/"

hugo_baseof_file = "#{base_hugo_site}#{ocaml_layouts}baseof.html"
modern_files = Path.wildcard "#{base_hugo_site}#{static_dir}modern*"
style_files = Path.wildcard "#{base_hugo_site}#{static_dir}styles.*"

if File.exists?(hugo_baseof_file) do
	case File.rm hugo_baseof_file do
		:ok ->
			IO.puts "  Removed: #{hugo_baseof_file}"
		{:error, reason} ->
			IO. puts "  Error: #{reason}"
	end
end

unless Enum.empty?(modern_files) do
	modern_files
	|> Enum.each(fn file ->
		case File.rm file do
			:ok ->
				IO.puts "  Removed: #{file}"
			{:error, reason} ->
				IO. puts "  Error: #{reason}"
		end
	end)
end

unless Enum.empty?(style_files) do
	style_files
	|> Enum.each(fn file ->
		case File.rm file do
			:ok ->
				IO.puts "  Removed: #{file}"
			{:error, reason} ->
				IO. puts "  Error: #{reason}"
		end
	end)
end
IO.puts "Done.\n"

# ------------------------------------

IO.puts "Copying new modern docs over to Hugo site ..."

modern_docs_baseof_file = "#{release_dir}baseof.html"

case File.rename(modern_docs_baseof_file, hugo_baseof_file) do
	:ok ->
		IO.puts "  Moved: #{modern_docs_baseof_file} to #{hugo_baseof_file}"
	{:error, reason} ->
		IO. puts "  Error: #{reason}"
end

Path.wildcard("#{release_dir}*")
|> Enum.each(fn file ->
	case File.rename(file, "#{base_hugo_site}#{static_dir}#{Path.basename(file)}") do
		:ok ->
			IO.puts "  Moved: #{file} to #{base_hugo_site}#{static_dir}"
		{:error, reason} ->
			IO. puts "  Error: #{reason}"
	end
end)

IO.puts "Done.\n"

IO.puts "Finished!!!"