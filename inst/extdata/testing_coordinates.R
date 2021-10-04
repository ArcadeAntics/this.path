# based on a 1920 x 1080 monitor


{
    Rautogui::setPAUSE(0.5)
    this.path:::Rgui("--vanilla", wait = FALSE, quiet = TRUE)
    Sys.sleep(0.1)


    # 'File' button
    # Rautogui::pos(   0,   23)
    # Rautogui::pos(  31,   23)
    # Rautogui::pos(   0,   41)
    # Rautogui::pos(  31,   41)
    Rautogui::pos(  15,   32, duration = 0.5)
    Rautogui::left()


    # 'New script' button
    # Rautogui::pos(   3,   67)
    # Rautogui::pos( 216,   67)
    # Rautogui::pos(   3,   88)
    # Rautogui::pos( 216,   88)
    Rautogui::pos( 109,   77)
    Rautogui::left()


    # Select R Console
    # Rautogui::pos(   0,   78)
    # Rautogui::pos( 680,   78)
    # Rautogui::pos(   0,  549)
    # Rautogui::pos( 680,  549)
    Rautogui::pos( 340,  313)
    Rautogui::left()
}
