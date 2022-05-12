import pyautogui, sys
pyautogui.PAUSE = 0.2
duration = 0.33


w = pyautogui.getActiveWindow()


if not w.title.startswith('RGui'):
    raise ValueError('RGui is not the active window')


pyautogui.leftClick(  19,   40)  # 'File' button
pyautogui.leftClick( 137,  125)  # 'Open script...' button
pyautogui.write(sys.argv[1] + '\n')
pyautogui.leftClick( 404,  386)  # Select R Console
pyautogui.write('fun()\n')


pyautogui.leftClick(  19,   40)  # 'File' button
pyautogui.leftClick( 137,   97)  # 'New script' button
pyautogui.leftClick( 404,  386)  # Select R Console
pyautogui.write('fun()\n')


w.close()

