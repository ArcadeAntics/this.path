# based on a 1920 x 1080 monitor


{
    Rautogui::setPAUSE(0.1)
    duration <- 0.33
    this.path:::Rgui("--vanilla", wait = FALSE, quiet = TRUE) ; Sys.sleep(0.2)


    # 'File' button
    Rautogui::pos (   0,   23, duration = duration)
    Rautogui::pos (  31,   23, duration = duration)
    Rautogui::pos (  31,   41, duration = duration)
    Rautogui::pos (   0,   41, duration = duration)
    Rautogui::pos (   0,   23, duration = duration)
    Rautogui::left(  15,   32, duration = duration)


    # 'New script' button
    Rautogui::pos (   3,   67, duration =        0)
    Rautogui::pos ( 216,   67, duration = duration)
    Rautogui::pos ( 216,   88, duration = duration)
    Rautogui::pos (   3,   88, duration = duration)
    Rautogui::pos (   3,   67, duration = duration)
    Rautogui::left( 109,   77, duration = duration)


    # Select R Console
    Rautogui::pos (   0,   78, duration =        0)
    Rautogui::pos ( 680,   78, duration = duration)
    Rautogui::pos ( 680,  549, duration = duration)
    Rautogui::pos (   0,  549, duration = duration)
    Rautogui::pos (   0,   78, duration = duration)
    Rautogui::left( 340,  313, duration = duration)


    Rautogui::type("q()\n", interval = 0.25)
}
