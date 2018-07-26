#=============================================================================================
# This script parses an h2k file from the ERS database, and deletes personal information;  
# Original file name consist a unique NRCan ID for each house, so, this script rename the 
# original file and copy that in a separate directory. A reference CSV file is generated to 
# hold the original name of h2k files.
#=============================================================================================

require 'rexml/document'
require 'fileutils'
require 'optparse'
require 'date'

include REXML   # This allows for no "REXML::" prefix to REXML methods

$BuilderCode = Hash.new(0)		# A hash to save the unique builder codes
$CountBuilder = Hash.new(1000)	# A hash to generate a new code for each builder, Privacy concerns

def get_elements_from_filename(filename)
  h2kFile = File.open(filename)
  $XMLdoc = Document.new(h2kFile)
  return $XMLdoc.elements()
end

def process_xml_file(h2kelements)
  locationText = "HouseFile/ProgramInformation"
  h2kelements.each(locationText) do |general|
    # Energy advisor "EA" name and company
    general.elements["File"].attributes["evaluationDate"] = "#{Date.today}"
    general.elements["File"].elements["Identification"].text = "ERS-#{$Count}"
    general.elements["File"].elements["PreviousFileId"].text = ""
    general.elements["File"].elements["EnrollmentId"].text = ""
    general.elements["File"].elements["TaxNumber"].text = ""
    general.elements["File"].elements["EnteredBy"].text = "Energy Advisor"
    general.elements["File"].elements["Company"].text = "Private"

    province = general.elements["Weather"].elements["Region"].elements["English"].text.split.map(&:chr).join

    $Builder = general.elements["File"].elements["BuilderName"].text.lstrip.upcase
    $Builder.slice!(5..-1)
    if (!$BuilderCode.has_key?("#{$Builder}"))
      if $Builder =~ /H0000/							# One time builders, no code for individual builders.
        $BuilderCode["#{$Builder}"] = "H0000"
      else
        $BuilderCode["#{$Builder}"] = "#{province}-#{$CountBuilder["#{province}"]}"	# Generate a new code for builders based on the Province
        $CountBuilder["#{province}"] += 1
      end
    end
    general.elements["File"].elements["BuilderName"].text = $BuilderCode["#{$Builder}"]
    general.elements["File"].add_element("HomeownerAuthorizationId"," \n")

    # Client first and last name
    general.elements["Client"].elements["Name"].elements["First"].text = ""
    general.elements["Client"].elements["Name"].elements["Last"].text = ""

    # Client phone number
    general.elements["Client"].elements["Telephone"].text = ""

    # Client address
    general.elements["Client"].elements["StreetAddress"].elements["Street"].text = ""
    if general.elements["Client"].elements["StreetAddress"].elements["UnitNumber"] != nil
      general.elements["Client"].elements["StreetAddress"].elements["UnitNumber"].text = ""
    else
      general.elements["Client"].elements["StreetAddress"].add_element("UnitNumber","")
    end
    general.elements["Client"].elements["StreetAddress"].elements["City"].text = ""
    general.elements["Client"].elements["StreetAddress"].elements["Province"].text = general.elements["Weather"].elements["Region"].elements["English"].text
	 if general.elements["Client"].elements["StreetAddress"].elements["Province"].text == "YUKON TERRITORY"
		general.elements["Client"].elements["StreetAddress"].elements["Province"].text = "YUKON"
    end
	 postalCode = general.elements["Client"].elements["StreetAddress"].elements["PostalCode"].text
    postalCode.slice!(3..-1)
    #general.elements["Client"].elements["StreetAddress"].elements["PostalCode"].text = "#{postalCode}"  # Can be used to keep first three letters of postal code
    general.elements["Client"].elements["StreetAddress"].elements["PostalCode"].text = ""

    # Mailing address
    general.elements["Client"].elements["MailingAddress"].elements["Name"].text = ""
    general.elements["Client"].elements["MailingAddress"].elements["Street"].text = ""
    if general.elements["Client"].elements["MailingAddress"].elements["UnitNumber"] != nil
      general.elements["Client"].elements["MailingAddress"].elements["UnitNumber"].text = ""
    else
      general.elements["Client"].elements["MailingAddress"].add_element("UnitNumber","")
    end
    general.elements["Client"].elements["MailingAddress"].elements["City"].text = ""
    general.elements["Client"].elements["MailingAddress"].elements["Province"].text = ""
    general.elements["Client"].elements["MailingAddress"].elements["PostalCode"].text = ""

    general.elements["Justifications"].elements["PossessionDate"].attributes["selected"] = "false"

  end

  # Information field
  locationText = "HouseFile/ProgramInformation/Information"
  if h2kelements[locationText] != nil
    locationText = "HouseFile/ProgramInformation"
    h2kelements[locationText].delete_element("Information")
  end

  # Delete HOC input data
  locationText = "HouseFile/Program/Options/HouseholdOperatingConditions"
  if h2kelements[locationText] != nil
    locationText = "HouseFile/Program/Options"
    h2kelements[locationText].delete_element("HouseholdOperatingConditions")
  end


=begin
  locationText = "HouseFile/AllResults/Results"
  h2kelements.each(locationText) do |general|
    houseCode = general.attributes["houseCode"]
    puts "#{houseCode}"
    if houseCode == "HOC"
    #  general.delete_attribute("houseCode")
    #  general.delete_attribute("sha256")
    #  general.delete_element("Labels")
      general.delete_element("Annual")
      general.delete_element("Monthly")
      general.delete_element("Other")
    end
  end
=end


  locationText = "HouseFile/Program"
  h2kelements.each(locationText) do |general|
    general.elements["Options"].elements["Main"].elements["Vermiculite"].attributes["code"] = "1"
    general.elements["Options"].elements["Main"].elements["Vermiculite"].elements["English"].text = "Unknown"
    general.elements["Options"].elements["Main"].elements["Vermiculite"].elements["French"].text = "Inconnu"
	 
	 # Delete Household operating conditions (HOC) input data 
	 locationText = "HouseFile/Program/Options"
	 if h2kelements[locationText].elements["Main"].attributes["applyHouseholdOperatingConditions"] == "true"
			h2kelements[locationText].delete_element("HouseholdOperatingConditions")
	 end
	 
	 # Delete ERS input data (ROC, Water conservation, Atypical loads, Reference House)
	 if h2kelements[locationText].elements["Main"].attributes["applyReducedOperatingConditions"] == "true"
			h2kelements[locationText].delete_element("ReducedOperatingConditions")
	 end
	 if h2kelements[locationText].elements["Main"].attributes["waterConservation"] == "true"
			h2kelements[locationText].delete_element("WaterConservation")
	 end
	 if h2kelements[locationText].elements["Main"].attributes["atypicalElectricalLoads"] == "true"
			h2kelements[locationText].delete_element("AtypicalElectricalLoads")
	 end
	 if h2kelements[locationText].elements["Main"].attributes["referenceHouse"] == "true"
			h2kelements[locationText].delete_element("ReferenceHouse")
	 end

    # EnerGuidle rating system - Options (Disable All)
    general.elements["Options"].elements["Main"].attributes["applyHouseholdOperatingConditions"] = "false"
    general.elements["Options"].elements["Main"].attributes["applyReducedOperatingConditions"] = "false"
    general.elements["Options"].elements["Main"].attributes["atypicalElectricalLoads"] = "false"
    general.elements["Options"].elements["Main"].attributes["referenceHouse"] = "false"
    general.elements["Options"].elements["Main"].attributes["waterConservation"] = "false"

    # TSV results
    general.elements["Results"].elements["Tsv"].elements["ClientCity"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["ClientAddr"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Builder"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["EntryBy"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["ClientName"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Telephone"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["ClientPcode"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["EntryDate"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["PreviousFileID"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["MailAddr"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["MailCity"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["MailRegion"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["MailPCode"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["TaxNumber"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Info1"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Info2"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Info3"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Info4"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Info5"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Info6"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Info7"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Info8"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Info9"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["Info10"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["HOCERSRating"].attributes["value"] = ""
    general.elements["Results"].elements["Tsv"].elements["HOCUGRERSRating"].attributes["value"] = ""
end

  # ERS results
  locationText = "HouseFile/Program/Results/*/*"
  h2kelements.each(locationText) do |general|
    if general.name =~ /HOC[a-zA-Z]/
      general.attributes["value"] = ""
    end
  end

  # Save changes to the XML doc in existing working H2K file (overwrite original)
#  stream_out (" Overwriting: #{$gWorkingModelFile} \n")
  newXMLFile = File.open($gWorkingModelFile, "w")
  $XMLdoc.write(newXMLFile)
  newXMLFile.close
end

destDirectory = "C:/EGH-Database/dataForRelease/"
originDirectory = "C:/EGH-Database/Archetypes/"
FileUtils.rm_f(Dir["#{destDirectory}*"])
$Count = 1000
summaryFile = File.open("#{destDirectory}Summary.csv", "w")
summaryFile.write("New House ID, Original House ID, Builder, Builder Key\n")
Dir.glob(originDirectory+"*.H2K").each do |h2kFile|

	h2kFile.slice!("#{originDirectory}")
  FileUtils.cp("#{originDirectory}#{h2kFile}","#{destDirectory}")
  $gWorkingModelFile = destDirectory+"ERS-"+"#{$Count}.H2K"
  File.rename("#{destDirectory}#{h2kFile}","#{$gWorkingModelFile}")
 # $gWorkingModelFile = destDirectory+"ERS-"+"#{count}"
  h2kElements = get_elements_from_filename("#{$gWorkingModelFile}")

  process_xml_file(h2kElements)

  summaryFile.write("ERS-#{$Count}, #{h2kFile}, #{$Builder}, #{$BuilderCode["#{$Builder}"]}\n")
  $Count +=1

end
summaryFile.close()
