package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Transparency;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;




/**
 * Class thats creates a custom panel that has an image in one of 3 states(centered, stretched, tiled) as its background. 
 * Used by the DialogNewMolecule dialog.
 * 
 * @author Dragana Jovanovska  
 */ 
public class BackgroundImagePanel extends JPanel {
   
	
	public final static int CENTERED = 0;

    public final static int STRETCHED = 1;

    public final static int TILED = 2;

    private BufferedImage backgroundImage;
    private int backgroundPosition;
    private BufferedImage stretchedImage;
    private static final int TOP = 0;
    private static final int LEFT = 0;
    private static final int CENTER = -1;
    private static final int BOTTOM = -2;
    private static final int RIGHT = -2;
    private Color gradientColor1;
    private Color gradientColor2;
    private int gradientX1 = 0;
    private int gradientX2 = 0;
    private int gradientY1 = 0;
    private int gradientY2 = 0;
    private boolean cyclic = false;

    
    
      
    public void setBackgroundImage(BufferedImage image) {
        backgroundImage = image;
        stretchedImage = null;
    }

    public void setBackgroundImage(String path) throws IOException {
    	
    	boolean b=SwingUtilities.isEventDispatchThread();
    	
        setBackgroundImage(Utils.createBufferedImage((path)));
    }
    
 

        
    public void setBackgroundPosition(int position) {
        backgroundPosition = position;
    }

    public void setBackgroundGradient(Color color1, Color color2) {
        setBackgroundGradient(color1, CENTER, TOP, color2, CENTER, BOTTOM, true);
    }

    public void setBackgroundGradient(Color color1, int x1, int y1,
            Color color2, int x2, int y2, boolean cyclic) {
        setBackgroundImage((BufferedImage) null);
        gradientColor1 = color1;
        gradientColor2 = color2;
        gradientX1 = x1;
        gradientY1 = y1;
        gradientX2 = x2;
        gradientY2 = y2;
        this.cyclic = cyclic;
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g;
        if (backgroundImage != null) {
            switch (backgroundPosition) {
            case CENTERED:
                paintCenteredImage(g2);
                break;
            case STRETCHED:
                paintStretchedImage(g2);
                break;
            case TILED:
                paintTiledImage(g2);
                break;
            }
        }
        else if ((gradientColor1 != null) && (gradientColor2 != null)) {
            paintGradient(g2);
        }
    }

    private void paintGradient(Graphics2D g2) {
        int x1 = gradientX1;
        if (x1 == CENTER)
            x1 = getWidth() / 2;
        if (x1 == RIGHT)
            x1 = getWidth();
        int y1 = gradientY1;
        if (y1 == CENTER)
            y1 = getHeight() / 2;
        if (y1 == BOTTOM)
            y1 = getHeight();
        int x2 = gradientX2;
        if (x2 == CENTER)
            x2 = getWidth() / 2;
        if (x2 == RIGHT)
            x2 = getWidth();
        int y2 = gradientY2;
        if (y2 == CENTER)
            y2 = getHeight() / 2;
        if (y2 == BOTTOM)
            y2 = getHeight();
        g2.setPaint(new GradientPaint(x1, y1, gradientColor1, x2, y2,
                gradientColor2, cyclic));
        g2.fillRect(0, 0, getWidth(), getHeight());
    }

    private void paintCenteredImage(Graphics2D g2) {
        int x = (getWidth() - backgroundImage.getWidth()) / 2;
        int y = (getHeight() - backgroundImage.getHeight()) / 2;
        g2.drawImage(backgroundImage, null, x, y);
    }

    private void paintTiledImage(Graphics2D g2) {
        int tileWidth = backgroundImage.getWidth();
        int tileHeight = backgroundImage.getHeight();
        for (int x = 0; x < getWidth(); x += tileWidth) {
            for (int y = 0; y < getHeight(); y += tileHeight) {
                g2.drawImage(backgroundImage, null, x, y);
            }
        }
    }

    private void paintStretchedImage(Graphics2D g2) {
        if (stretchedImage == null) {
            // If it's the first time the image is streched.
            stretchedImage = createScaledImage();
        }
        else if ((stretchedImage.getWidth() != getWidth())
                || (stretchedImage.getHeight() != getHeight())) {
            // If the size of the panel has changed the image has to be streched
            // again.
            stretchedImage = createScaledImage();
        }
        g2.drawImage(stretchedImage, null, 0, 0);
    }

    private BufferedImage createScaledImage() {
        GraphicsConfiguration gc = getGraphicsConfiguration();
        BufferedImage newImage = gc.createCompatibleImage(getWidth(),
                getHeight(), Transparency.TRANSLUCENT);

        Graphics g = newImage.getGraphics();
        int width = backgroundImage.getWidth();
        int height = backgroundImage.getHeight();
        g.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), 0, 0,
                width, height, null);
        g.dispose();

        return newImage;
    }
    
   /* public static boolean hasAlpha(Image image) {
        // If buffered image, the color model is readily available
        if (image instanceof BufferedImage) {
            BufferedImage bimage = (BufferedImage)image;
            return bimage.getColorModel().hasAlpha();
        }
    
        // Use a pixel grabber to retrieve the image's color model;
        // grabbing a single pixel is usually sufficient
         PixelGrabber pg = new PixelGrabber(image, 0, 0, 1, 1, false);
        try {
            pg.grabPixels();
        } catch (InterruptedException e) {
        }
    
        // Get the image's color model
        ColorModel cm = pg.getColorModel();
        return cm.hasAlpha();
    }
    */
    /*public static BufferedImage toBufferedImage(Image image) {
        if (image instanceof BufferedImage) {
            return (BufferedImage)image;
        }
    
        // This code ensures that all the pixels in the image are loaded
        image = new ImageIcon(image).getImage();
    
        // Determine if the image has transparent pixels; for this method's
        // implementation, see e661 Determining If an Image Has Transparent Pixels
        boolean hasAlpha = hasAlpha(image);
    
        // Create a buffered image with a format that's compatible with the screen
        BufferedImage bimage = null;
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        try {
            // Determine the type of transparency of the new buffered image
            int transparency = Transparency.OPAQUE;
            if (hasAlpha) {
                transparency = Transparency.BITMASK;
            }
    
            // Create the buffered image
            GraphicsDevice gs = ge.getDefaultScreenDevice();
            GraphicsConfiguration gc = gs.getDefaultConfiguration();
            bimage = gc.createCompatibleImage(
                image.getWidth(null), image.getHeight(null), transparency);
        } catch (HeadlessException e) {
            // The system does not have a screen
        }
    
        if (bimage == null) {
            // Create a buffered image using the default color model
            int type = BufferedImage.TYPE_INT_RGB;
            if (hasAlpha) {
                type = BufferedImage.TYPE_INT_ARGB;
            }
            bimage = new BufferedImage(image.getWidth(null), image.getHeight(null), type);
        }
    
        // Copy image to buffered image
        Graphics g = bimage.createGraphics();
    
        // Paint the image onto the buffered image
        g.drawImage(image, 0, 0, null);
        g.dispose();
    
        return bimage;
    }*/

    /*public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        JFrame frame = new JFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        BackgroundImagePanel panel = new BackgroundImagePanel();
        String imagePath=null;
        try {
        	imagePath = Utils.getFilePath("images/Chemistry.jpg");
        	panel.setBackgroundImage(imagePath);
        }
        catch (IOException e) {
        	 e.printStackTrace();
        }
        panel.setBackgroundPosition(BackgroundImagePanel);
      //  panel.setBackgroundGradient(Color.green, Color.white);
        panel.setLayout(new FlowLayout());
        panel.setPreferredSize(new Dimension(300, 300));
        frame.getContentPane().add(panel);
        frame.pack();
        frame.setVisible(true);
    }*/
}
