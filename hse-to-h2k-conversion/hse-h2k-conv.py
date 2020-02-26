import pywinauto
from pywinauto import Desktop, Application
from pywinauto.keyboard import send_keys
import os
import time
from pywinauto.timings import WaitUntil

# set paths in one spot to simplify life.
script_folder_path = os.path.dirname(os.path.realpath(__file__))
files_to_convert_folder = os.path.abspath(os.path.join(script_folder_path,'files_to_convert'))

# Get list of hse files names into an array/list.
hse_list = []
for file in os.listdir(files_to_convert_folder):
    if file.endswith(".HSE"):
        #strip extention and add to
        hse_list.append(os.path.splitext(file)[0])
#Output list of HSE prefixes to console.
print(*hse_list,  sep = "\n")

# Will create an iterator later.. for now just using first HSE file.
#hse_name = hse_list[0]
for hse_name in hse_list:
	app = Application().start("C:\HOT2000 v11.3\HOT2000.exe")
	app.HOT2000.menu_select("File->Open")

#hardcoding input hse folder path for now.
	app.Open.Edit.type_keys("C:\\h2k_upgrader\\files_to_convert\\{}.HSE".format(hse_name))
	app.Open.Open.click()

# spams enter to get by errors and messages. Hopefully 8 is enough?
	send_keys('{ENTER 8}')

# Sleep while H2k does its thing.
	app.top_window().maximize()
	time.sleep(1.0)
# Hot 2000 dialog name has changed.
	new_name = "HOT2000 - [{} - General]".format(hse_name)
	for y_coordinate in [130, 150, 170, 190, 210, 230, 250, 270]:
		pywinauto.mouse.click(button='left', coords=(50, y_coordinate))
		time.sleep(1.0)
		pywinauto.mouse.click(button='left', coords=(300, 130))
		send_keys('{ENTER}')
		time.sleep(1.0)
#Switch to summary view
	pywinauto.mouse.click(button='left', coords=(50, 100))
	time.sleep(1.0)
	
	for y_coordinate in [100, 115, 130, 150, 170, 205, 220, 235, 260, 280]:
		pywinauto.mouse.click(button='left', coords=(50, y_coordinate))
		send_keys('{ENTER 20}')
		time.sleep(1.0)
	
	#hardcoding ouput h2k folder path for now.
	app[new_name].menu_select("File->Save As")
	app.SaveAs.Edit.type_keys("C:\\ERS_Database\\hse-to-h2k-conversion\\converted_files\\{}.h2k".format(hse_name))
	app.SaveAs.Save.click()
	time.sleep(1.0)
	send_keys('{ENTER 30}')
	time.sleep(1.0)
	app[new_name].menu_select("File->Exit")



