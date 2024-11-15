import tkinter as tk

class RoundButton(tk.Canvas):
    def __init__(self, parent, text, command=None, bg="#000000", fg="#ffffff", font=("Helvetica", 24, "bold"), radius=25, width=150, height=50, **kwargs):
        # Remove background color of the Canvas by setting `background` to match the parent
        tk.Canvas.__init__(self, parent, highlightthickness=0, bg=parent.cget("bg"), **kwargs)
        
        self.command = command
        self.radius = radius
        self.bg = bg
        self.fg = fg
        self.font = font
        self.width = width
        self.height = height
        
        # Draw rounded rectangle
        self.create_rounded_rectangle(5, 5, self.width, self.height, radius=radius, fill=bg, outline=bg)
        
        # Place the text in the center
        self.text_id = self.create_text(
            self.width // 2,
            self.height // 2,
            text=text,
            font=font,
            fill=fg
        )
        
        # Bind click event
        self.bind("<Button-1>", self.on_click)
    
    def create_rounded_rectangle(self, x1, y1, x2, y2, radius=25, **kwargs):
        """Create a rounded rectangle on the canvas."""
        self.create_arc(x1, y1, x1 + radius * 2, y1 + radius * 2, start=90, extent=90, style='pieslice', **kwargs)
        self.create_arc(x2 - radius * 2, y1, x2, y1 + radius * 2, start=0, extent=90, style='pieslice', **kwargs)
        self.create_arc(x2 - radius * 2, y2 - radius * 2, x2, y2, start=270, extent=90, style='pieslice', **kwargs)
        self.create_arc(x1, y2 - radius * 2, x1 + radius * 2, y2, start=180, extent=90, style='pieslice', **kwargs)
        
        self.create_rectangle(x1 + radius, y1, x2 - radius, y2, **kwargs)
        self.create_rectangle(x1, y1 + radius, x2, y2 - radius, **kwargs)
    
    def on_click(self, event=None):
        """Trigger the command if available."""
        if self.command:
            self.command()
