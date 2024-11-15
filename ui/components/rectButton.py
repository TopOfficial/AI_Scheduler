import tkinter as tk

class RectButton(tk.Canvas):
    def __init__(self, parent, text, command=None, bg="#000000", fg="#ffffff", font=("Helvetica", 24, "bold"), width=150, height=50, **kwargs):
        # Remove background color of the Canvas by setting `background` to match the parent
        tk.Canvas.__init__(self, parent, highlightthickness=0, bg=parent.cget("bg"), **kwargs)
        
        self.command = command
        self.bg = bg
        self.fg = fg
        self.font = font
        self.width = width
        self.height = height
        
        # Draw a rectangle (no rounded corners)
        self.create_rectangle(0, 0, self.width, self.height, fill=bg, outline=bg)
        
        # Place the text in the center of the button
        self.text_id = self.create_text(
            self.width // 2,
            self.height // 2,
            text=text,
            font=font,
            fill=fg
        )
        
        # Bind click event
        self.bind("<Button-1>", self.on_click)
    
    def on_click(self, event=None):
        """Trigger the command if available."""
        if self.command:
            self.command()