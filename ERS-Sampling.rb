require 'fileutils'
require 'CSV'
require 'rexml/document'
require 'optparse'

include REXML  

$SD_CapHouseMarketHash={
    "AT" => 15,
    "GTA" => 15,
    "ON-OTT" => 3,
	 "QC" => 15,
    "PR-6" => 0,
    "PR-7A" => 15,
    "PR-7B" => 0,
    "BC-LM" => 15,
    "BC-Int" => 15,
    "North" => 15,
}

$DR_CapHouseMarketHash={
    "AT" => 15,
    "GTA" => 15,
    "ON-OTT" => 3,
	 "QC" => 15,
    "PR-6" => 0,
    "PR-7A" => 15,
    "PR-7B" => 0,
    "BC-LM" => 15,
    "BC-Int" => 15,
    "North" => 15,
}

$SD_Roof={
    "Attic/hip" => 0,
    "Attic/gable" => 0,
    "Flat" => 3,
    "Scissor" => 3,
    "Cathedral" => 3,
}

$SD_Foundation={
    "Basement" => 0,
    "Slab" => 3,
    "Crawl" => 3,
    "Walkout" => 3,
}

$Categorize = FALSE
$count_SD = Hash.new(0)
$count_DR = Hash.new(0)
$roof_SD = Hash.new(0)
$foundation_SD = Hash.new(0)

def get_elements_from_filename(filename)
  h2kFile = File.open(filename)
  $XMLdoc = Document.new(h2kFile)
  return $XMLdoc.elements()
end

def process_xml_file(h2kelements,provCode,province,locationCode,location)
  locationText = "HouseFile/ProgramInformation"
  h2kelements.each(locationText) do |general|
    # Energy advisor "EA" name and company
    general.elements["Weather"].elements["Region"].attributes["code"] = "#{provCode}"
	 general.elements["Weather"].elements["Region"].elements["English"].text = "#{province}"
	 general.elements["Weather"].elements["Region"].elements["French"].text = "#{province}"
	 general.elements["Weather"].elements["Location"].attributes["code"] = "#{locationCode}"
	 general.elements["Weather"].elements["Location"].elements["English"].text = "#{location}"
	 general.elements["Weather"].elements["Location"].elements["French"].text = "#{location}"
	 general.elements["Client"].elements["StreetAddress"].elements["Province"].text = "#{province}"
	 
	 # File name
	 fileID = general.elements["File"].elements["Identification"].text
	 general.elements["File"].elements["PreviousFileId"].text = "#{fileID}"
	 general.elements["File"].elements["Identification"].text = "PROXY-#{fileID}"
	 
# Save changes to the XML doc in existing working H2K file (overwrite original)
#  stream_out (" Overwriting: #{$gWorkingModelFile} \n")
  newXMLFile = File.open($gWorkingModelFile, "w")
  $XMLdoc.write(newXMLFile)
  newXMLFile.close

  end
end

if ARGV.empty? then
  puts "Please enter selection seed!"
  exit()
end

selectionSeed = Random.new(ARGV[0].to_i)
selectionNumber = selectionSeed.rand(1000)
binTypes = ["general", "small-house", "large-house", "bunker-house", "green-house"]
roofTypes = ["Attic/hip", "Attic/gable", "Flat", "Scissor", "Cathedral"]
foundationTypes = ["Basement", "Slab", "Crawl", "Walkout"]
regions = ["AT", "GTA", "ON-OTT", "PR-6", "PR-7A", "PR-7B", "BC-LM", "BC-Int", "North"]
binTypes.each do |selection|
  if selection =~ /general/
    $count_SD = Hash.new(0)
    $count_DR = Hash.new(0)
    $roof_SD = Hash.new{|h,k| h[k]=Hash.new(&h.default_proc) }
    $foundation_SD = Hash.new{|h,k| h[k]=Hash.new(&h.default_proc) }
    $roof_DR = Hash.new{|h,k| h[k]=Hash.new(&h.default_proc) }
    $foundation_DR = Hash.new{|h,k| h[k]=Hash.new(&h.default_proc) }
    regions.each do |region|
      roofTypes.each do |roof|
        $roof_SD[roof][region] = 0
        $roof_DR[roof][region] = 0
      end
      foundationTypes.each do |foundation|
        $foundation_SD[foundation][region] = 0
        $foundation_DR[foundation][region] = 0
      end
    end

    if selection =~ /general/
      inputFile = File.open("C:\\EGH-Database\\select.data").sort_by{selectionSeed.rand(1000)}  #sort_by{selectionNumber}
      binLabel = "G"
    elsif selection =~ /small-house/
      inputFile = File.open("C:\\EGH-Database\\Small-houses.csv").sort_by{selectionSeed.rand(1000)}  #sort_by{selectionNumber}
      binLabel = "SH"
    elsif selection =~ /large-house/
      inputFile = File.open("C:\\EGH-Database\\Large-houses.csv").sort_by{selectionSeed.rand(1000)}  #sort_by{selectionNumber}
      binLabel = "LH"
    elsif selection =~ /bunker-house/
      inputFile = File.open("C:\\EGH-Database\\Bunker-houses.csv").sort_by{selectionSeed.rand(1000)}  #sort_by{selectionNumber}
      binLabel = "BH"
    elsif selection =~ /green-house/
      inputFile = File.open("C:\\EGH-Database\\Green-houses.csv").sort_by{selectionSeed.rand(1000)}  #sort_by{selectionNumber}
      binLabel = "GH"
    end
    inputKey = CSV.read("C:/EGH-Database/dataForRelease/Summary.csv")

    if (! Dir.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\"))
      FileUtils::mkdir "C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\"
    else
      FileUtils.rm_f(Dir["C:/EGH-Database/bins/bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}/*.H2K"])
    end

    inputFile.each do |line|
      fileEntry=line.split()
      type=fileEntry[0].to_s
      region=fileEntry[1].to_s
      h2kFile=fileEntry[2]
      roofType=fileEntry[3].to_s
      foundationType=fileEntry[4].to_s
      storeys=fileEntry[5].to_i

      h2kForHTAP = inputKey.find_all{|e1| e1[1]==" #{h2kFile}"}[0][0].to_s # if the original names with .H2K are used
      # h2kForHTAP = inputKey.find_all{|e1| e1[0]=="#{h2kFile}"}[0][0].to_s  # if ERS names without .H2K are used
      # puts "#{h2kFile},#{h2kForHTAP},#{roofType},#{foundationType},#{$foundation_SD[foundationType][region]},#{$SD_Foundation[foundationType]}"


      if(type=~/SD/ && $count_SD[region]<$SD_CapHouseMarketHash[region] && ($roof_SD[roofType][region]<$SD_Roof[roofType] || $foundation_SD[foundationType][region]<$SD_Foundation[foundationType]))
        if $roof_SD[roofType][region]<$SD_Roof[roofType]
          if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
            FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
            $count_SD[region]+=1
          end
          $roof_SD[roofType][region]+=1
        end
        #FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\")
        if $foundation_SD[foundationType][region]<$SD_Foundation[foundationType]
          if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
            FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
            $count_SD[region]+=1
          end
          $foundation_SD[foundationType][region]+=1
        end
      elsif(type=~/DR/ && $count_DR[region]<$DR_CapHouseMarketHash[region] && ($roof_DR[roofType][region]<$SD_Roof[roofType] || $foundation_DR[foundationType][region]<$SD_Foundation[foundationType]))
        #FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\")

        if $roof_DR[roofType][region]<$SD_Roof[roofType]
          if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
            FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
            $count_DR[region]+=1
          end
          $roof_DR[roofType][region]+=1
        end
        if $foundation_DR[foundationType][region]<$SD_Foundation[foundationType]
          if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
            FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
            $count_DR[region]+=1
          end
          $foundation_DR[foundationType][region]+=1
        end
      end
    end

    inputFile.each do |line|
      fileEntry=line.split()
      type=fileEntry[0].to_s
      region=fileEntry[1].to_s
      h2kFile=fileEntry[2]
      roofType=fileEntry[3].to_s
      foundationType=fileEntry[4].to_s
      storeys=fileEntry[5].to_i

      h2kForHTAP = inputKey.find_all{|e1| e1[1]==" #{h2kFile}"}[0][0].to_s # if the original names with .H2K are used
      # h2kForHTAP = inputKey.find_all{|e1| e1[0]=="#{h2kFile}"}[0][0].to_s  # if ERS names without .H2K are used
      # puts "#{h2kFile},#{h2kForHTAP},#{roofType},#{foundationType},#{$foundation_SD[foundationType][region]},#{$SD_Foundation[foundationType]}"


      if(type=~/SD/ && $count_SD[region]<$SD_CapHouseMarketHash[region] )
        if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\")
          $count_SD[region]+=1
        end
      elsif(type=~/DR/ && $count_DR[region]<$DR_CapHouseMarketHash[region])
        if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\")
          $count_DR[region]+=1
        end
      end
		
      if $Categorize
        if (! Dir.exist?("C:\\EGH-Database\\bins\\Categories\\SPECIAL-Form.#{roofType.gsub('/','-')}\\"))
          FileUtils::mkdir "C:\\EGH-Database\\bins\\Categories\\SPECIAL-Form.#{roofType.gsub('/','-')}\\"
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\Categories\\SPECIAL-Form.#{roofType.gsub('/','-')}\\")
        else
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\Categories\\SPECIAL-Form.#{roofType.gsub('/','-')}\\")
        end

        if (! Dir.exist?("C:\\EGH-Database\\bins\\Categories\\SPECIAL-Form.#{foundationType}\\"))
          FileUtils::mkdir "C:\\EGH-Database\\bins\\Categories\\SPECIAL-Form.#{foundationType}\\"
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\Categories\\SPECIAL-Form.#{foundationType}\\")
        else
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\Categories\\SPECIAL-Form.#{foundationType}\\")
        end
      end

    end
	 #-----------------------------------------------------------------
	 #	PROXY HOUSES
	 #-----------------------------------------------------------------
	 inputFile.each do |line|
      fileEntry=line.split()
      type=fileEntry[0].to_s
      region=fileEntry[1].to_s
      h2kFile=fileEntry[2]
      roofType=fileEntry[3].to_s
      foundationType=fileEntry[4].to_s
      storeys=fileEntry[5].to_i

      h2kForHTAP = inputKey.find_all{|e1| e1[1]==" #{h2kFile}"}[0][0].to_s # if the original names with .H2K are used
      # h2kForHTAP = inputKey.find_all{|e1| e1[0]=="#{h2kFile}"}[0][0].to_s  # if ERS names without .H2K are used
      # puts "#{h2kFile},#{h2kForHTAP},#{roofType},#{foundationType},#{$foundation_SD[foundationType][region]},#{$SD_Foundation[foundationType]}"
		#------------------------------------------------------------
		#	GTA
		#------------------------------------------------------------
		if(type=~/SD/ && $count_SD["GTA"]<$SD_CapHouseMarketHash["GTA"] && region =~ /PR-7A/ )
        if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\")
          $count_SD["GTA"]+=1

			 $gWorkingModelFile = "C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K"
			 h2kFile = File.open("#{$gWorkingModelFile}")
			 h2kElements = get_elements_from_filename("#{$gWorkingModelFile}")
			 process_xml_file(h2kElements,5,"ONTARIO",42,"TORONTO")
			end
      elsif(type=~/DR/ && $count_DR["GTA"]<$DR_CapHouseMarketHash["GTA"] && region =~ /PR-7A/)
        if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\")
          $count_DR["GTA"]+=1
			 
			 $gWorkingModelFile = "C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K"
			 h2kFile = File.open("#{$gWorkingModelFile}")
			 h2kElements = get_elements_from_filename("#{$gWorkingModelFile}")
			 process_xml_file(h2kElements,5,"ONTARIO",42,"TORONTO")
        end
      end
		#------------------------------------------------------------
		#	ON-OTT
		#------------------------------------------------------------
		if(type=~/SD/ && $count_SD["ON-OTT"]<15 && region =~ /PR-7A/ )
        if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\")
          $count_SD["ON-OTT"]+=1

			 $gWorkingModelFile = "C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K"
			 h2kFile = File.open("#{$gWorkingModelFile}")
			 h2kElements = get_elements_from_filename("#{$gWorkingModelFile}")
			 process_xml_file(h2kElements,5,"ONTARIO",36,"OTTAWA")
			end
      elsif(type=~/DR/ && $count_DR["ON-OTT"]<15 && region =~ /PR-7A/)
        if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\")
          $count_DR["ON-OTT"]+=1
			 
			 $gWorkingModelFile = "C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K"
			 h2kFile = File.open("#{$gWorkingModelFile}")
			 h2kElements = get_elements_from_filename("#{$gWorkingModelFile}")
			 process_xml_file(h2kElements,5,"ONTARIO",36,"OTTAWA")
        end
      end
		#------------------------------------------------------------
		#	QC
		#------------------------------------------------------------
		if(type=~/SD/ && $count_SD["QC"]<$SD_CapHouseMarketHash["QC"] && region =~ /PR-7A/ )
        if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\")
          $count_SD["QC"]+=1

			 $gWorkingModelFile = "C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K"
			 h2kFile = File.open("#{$gWorkingModelFile}")
			 h2kElements = get_elements_from_filename("#{$gWorkingModelFile}")
			 process_xml_file(h2kElements,6,"QUEBEC",46,"MONTREAL")
			end
      elsif(type=~/DR/ && $count_DR["QC"]<$DR_CapHouseMarketHash["QC"] && region =~ /PR-7A/)
        if ! File.exist?("C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K")
          FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\")
          $count_DR["QC"]+=1
			 
			 $gWorkingModelFile = "C:\\EGH-Database\\bins\\bin.#{binLabel}.#{selectionNumber}.seed-#{ARGV[0].to_i}\\#{h2kForHTAP}.H2K"
			 h2kFile = File.open("#{$gWorkingModelFile}")
			 h2kElements = get_elements_from_filename("#{$gWorkingModelFile}")
			 process_xml_file(h2kElements,6,"QUEBEC",46,"MONTREAL")
        end
      end
		#------------------------------------------------------------------
		
		end
		#------------------------------------------------------------
		#	END OF PROXY HOUSES
		#------------------------------------------------------------
		
  else

    if selection =~ /small-house/
      inputFile = File.open("C:\\EGH-Database\\Small-houses.csv").sort_by{selectionSeed.rand(1000)}  #sort_by{selectionNumber}
      binLabel = "SH"
    elsif selection =~ /large-house/
      inputFile = File.open("C:\\EGH-Database\\Large-houses.csv").sort_by{selectionSeed.rand(1000)}  #sort_by{selectionNumber}
      binLabel = "LH"
    elsif selection =~ /bunker-house/
      inputFile = File.open("C:\\EGH-Database\\Bunker-houses.csv").sort_by{selectionSeed.rand(1000)}  #sort_by{selectionNumber}
      binLabel = "BH"
    elsif selection =~ /green-house/
      inputFile = File.open("C:\\EGH-Database\\Green-houses.csv").sort_by{selectionSeed.rand(1000)}  #sort_by{selectionNumber}
      binLabel = "GH"
    end

    if (! Dir.exist?("C:\\EGH-Database\\bins\\SPECIAL-bin.#{binLabel}\\"))
      FileUtils::mkdir "C:\\EGH-Database\\bins\\SPECIAL-bin.#{binLabel}\\"
    else
      FileUtils.rm_f(Dir["C:/EGH-Database/bins/SPECIAL-bin.#{binLabel}/*.H2K"])
    end

    inputKey = CSV.read("C:/EGH-Database/dataForRelease/Summary.csv")

    inputFile.each do |line|
      fileEntry=line.split()
      type=fileEntry[0].to_s
      region=fileEntry[1].to_s
      h2kFile=fileEntry[2]

      h2kForHTAP = inputKey.find_all{|e1| e1[1]==" #{h2kFile}"}[0][0].to_s # if the original names with .H2K are used



      FileUtils.cp("C:\\EGH-Database\\dataForRelease\\#{h2kForHTAP}.H2K","C:\\EGH-Database\\bins\\SPECIAL-bin.#{binLabel}\\")
    end
  end


end
