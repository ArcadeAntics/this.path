# based on a 1920 x 1080 monitor


{
    essentials:::Rgui(c("--vanilla", "R_DEFAULT_PACKAGES=NULL"), wait = FALSE, quiet = TRUE)
    Sys.sleep(0.2)
    setwd(tempdir())
    FILE <- tempfile(fileext = ".R")
    file.create(FILE)
    essentials::python(command = essentials::dedent(r"{
        import pyautogui
        duration = 0.33


        # 'Open script' button
        pyautogui.moveTo   (   6,   48, duration = duration)
        pyautogui.moveTo   (  29,   48, duration = duration)
        pyautogui.moveTo   (  29,   71, duration = duration)
        pyautogui.moveTo   (   6,   71, duration = duration)
        pyautogui.moveTo   (   6,   48, duration = duration)
        pyautogui.leftClick(  17,   59, duration = duration)
        pyautogui.write("%s\n", interval = 0.25)


        # 'File' button
        pyautogui.leftClick( 323,  308, duration = duration)
        pyautogui.moveTo   (   0,   23, duration = duration)
        pyautogui.moveTo   (  31,   23, duration = duration)
        pyautogui.moveTo   (  31,   41, duration = duration)
        pyautogui.moveTo   (   0,   41, duration = duration)
        pyautogui.moveTo   (   0,   23, duration = duration)
        pyautogui.leftClick(  15,   32, duration = duration)


        # 'New script' button
        pyautogui.moveTo   (   3,   67, duration =        0)
        pyautogui.moveTo   ( 216,   67, duration = duration)
        pyautogui.moveTo   ( 216,   88, duration = duration)
        pyautogui.moveTo   (   3,   88, duration = duration)
        pyautogui.moveTo   (   3,   67, duration = duration)
        pyautogui.leftClick( 109,   77, duration = duration)


        # Select R Console
        ##pyautogui.moveTo   (   0,   78, duration =        0)
        ##pyautogui.moveTo   ( 680,   78, duration = duration)
        ##pyautogui.moveTo   ( 680,  549, duration = duration)
        ##pyautogui.moveTo   (   0,  549, duration = duration)
        ##pyautogui.moveTo   (   0,   78, duration = duration)
        ##pyautogui.leftClick( 340,  313, duration = duration)
        pyautogui.moveTo   (   8,  109, duration =        0)
        pyautogui.moveTo   ( 639,  109, duration = duration)
        pyautogui.moveTo   ( 639,  508, duration = duration)
        pyautogui.moveTo   (   8,  508, duration = duration)
        pyautogui.moveTo   (   8,  109, duration = duration)
        pyautogui.leftClick( 323,  308, duration = duration)


        pyautogui.write("q()\n", interval = 0.25)
    }" |> sprintf(basename(FILE))), quiet = TRUE)
    unlink(FILE)
}
