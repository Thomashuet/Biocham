package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.utils.Utils;
import net.java.balloontip.styles.BalloonTipStyle;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.image.BufferedImage;



/**
 * Class that creates a custom BalloonTipStyle component with defined image filepath to show, corner's size, position and colors. 
 * 
 * @author Dragana Jovanovska  
 */ 
public class InfoTipStyle extends BalloonTipStyle{

	private int arcWidth;
	private int arcHeight;

	private boolean topLeft = true;
	private boolean topRight = false;
	boolean bottomLeft = false;
	boolean bottomRight = true;

	private int borderThickness = 1;
	private boolean AAenabled = false;

	private Color topFillColor;
	private Color bottomFillColor;
	private Color borderColor;
	private String imageFilepath;
	 BufferedImage img;
	/**
	 * Constructor
	 * @param arcWidth			width of the rounded corner
	 * @param arcHeight			height of the rounded color
	 * @param borderColor		line color
	 * @param topFillColor		top color of the lineair gradient fill color
	 * @param bottomFillColor	bottom color of the lineair gradient fill color
	 */
	public InfoTipStyle(String filepath,int arcWidth, int arcHeight, Color topFillColor, Color bottomFillColor, Color borderColor) {
		imageFilepath=filepath;
		this.arcWidth = arcWidth;
		this.arcHeight = arcHeight;
		this.topFillColor = topFillColor;
		this.bottomFillColor = bottomFillColor;
		this.borderColor = borderColor;
		img = Utils.createBufferedImage(imageFilepath);
	}

	/**
	 * Sets the style for each corner.
	 * If true, this corner will be rounded; if false, it's just a regular corner
	 * @param topLeft
	 * @param topRight
	 * @param bottomLeft
	 * @param bottomRight
	 */
	public void setCornerStyles(boolean topLeft, boolean topRight, boolean bottomLeft, boolean bottomRight) {
		this.topLeft = topLeft;
		this.topRight = topRight;
		this.bottomLeft = bottomLeft;
		this.bottomRight = bottomRight;
	}

	/**
	 * Set the thickness of the balloon tip's border, in px
	 * @param thickness
	 */
	public void setBorderThickness(int thickness) {
		borderThickness = thickness;
	}

	/**
	 * Enable/disable anti-aliasing for this balloon tip
	 * @param enable	if true, AA is enabled; if false, the settings remain untouched
	 */
	public void enableAntiAliasing(boolean enable) {
		AAenabled = enable;
	}
	
	public Insets getBorderInsets(Component c) {
		if (flipY) {
			return new Insets(verticalOffset+arcHeight, arcWidth, arcHeight, arcWidth);
		} else {
			return new Insets(arcHeight, arcWidth, arcHeight+verticalOffset, arcWidth);
		}
	}

	public boolean isBorderOpaque() {
		return true;
	}

	public int getWidth(){
		return img.getWidth();
	}
	public int getHeight(){
		return img.getHeight();
	}
	public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
		
		Graphics2D g2d = (Graphics2D) g;
		int w = img.getWidth();
		int h =img.getHeight();
		BufferedImage bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
		Graphics gg = bi.getGraphics();
		gg.drawImage(img, 0, 0, null);
		/* Create a rescale filter op that makes the image 50% opaque */
		g2d.drawImage(bi,null, 0, 0);
		 
	}
	
	public int getMinimalHorizontalOffset() {
		return arcWidth + verticalOffset + borderThickness;
	}
}
