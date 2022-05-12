import pyautogui, sys
pyautogui.PAUSE = 0.2
duration = 0.33


w = pyautogui.getActiveWindow()


if not w.title.startswith('RGui'):
    raise ValueError('RGui is not the active window')


pyautogui.leftClick(  15,   32)  # 'File' button
pyautogui.leftClick( 109,   99)  # 'Open script...' button
pyautogui.write(sys.argv[1] + '\n')
pyautogui.leftClick( 323,  308)  # Select R Console
pyautogui.write('fun()\n')


pyautogui.leftClick(  15,   32)  # 'File' button
pyautogui.leftClick( 109,   77)  # 'New script' button
pyautogui.leftClick( 323,  308)  # Select R Console
pyautogui.write('fun()\n')


w.close()

