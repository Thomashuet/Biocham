package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.modelData.ParamTableMolecules;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;



/**
 * Class thats creates a custom JPanel component that holds an image that can be zoomable and saved to a file.
 * 
 * @author Dragana Jovanovska  
 */ 
public class CustomZoomableImagePanel extends JPanel implements ActionListener{


    private double m_zoom = 1.0;
    private double m_zoomPercentage;
    private Image m_image;
    private ImageIcon m_imageIcon;  
    private Cursor m_zoomCursor;
    private UniversalScrollPane m_srollPane;
    private ImageContainer m_imageContainer;
    private static Cursor defaultCursor=new Cursor(Cursor.DEFAULT_CURSOR);
   
    /**
     * Constructor
     * 
     * @param image
     * @param zoomPercentage
     */                
    public CustomZoomableImagePanel(Image image, double zoomPercentage)
    {
    	
    	boolean b=SwingUtilities.isEventDispatchThread();
    	
        m_image = image;
        m_zoomPercentage = zoomPercentage / 100;    
        
    }
    
    public CustomZoomableImagePanel(ImageIcon image, double zoomPercentage)
    {
    	
    	boolean b=SwingUtilities.isEventDispatchThread();
    	
    	
    	m_imageIcon = image;
        m_zoomPercentage = zoomPercentage / 100;
              
    }
    public void saveToImage(String filename, String ext) {
    	int w=1000;
    	int h=1000;
    	int molSize=((ParamTableMolecules)BiochamDynamicTree.currentModel.getMolecules().getParamTable()).getMoleculesModel().getMolecules().size();
    	if(molSize>20){
    		w+=10*molSize;
    		h+=10*molSize;
    	}
    	BufferedImage image=Utils.toBufferedImage(m_image,w,h);
    	
    	 try {
         	File img=new File(filename);
 			ImageIO.write(image, ext, img);
 			img=null;
 		} catch (IOException e) {
 			image=null; 			
 			e.printStackTrace();
 		}
 		image=null;
 		
    	/*int w =getPreferredSize().width, h=getPreferredSize().height; 
        BufferedImage image = new BufferedImage(w, h,BufferedImage.TYPE_INT_RGB);
        Graphics2D g2 = image.createGraphics();
        paint(g2);
        g2.dispose();    
        try {
        	File img=new File(filename);
			ImageIO.write(image, ext, img);
			img=null;
		} catch (IOException e) {
			image=null;
			g2=null;
			e.printStackTrace();
		}
		image=null;
		g2=null;*/
    	
	}
    
    
    
    /**
     * This method is overriden to draw the image
     * and scale the graphics accordingly
     */
    public void paintComponent(Graphics grp) 
    { 
        Graphics2D g2D = (Graphics2D)grp;
        g2D.setColor(Color.WHITE);
        g2D.fillRect(0, 0, getWidth(), getHeight());
        g2D.scale(m_zoom, m_zoom);
        g2D.drawImage(m_image, 0, 0, this); 
    }
    
     
    /**
     * This method is overriden to return the preferred size
     * which will be the width and height of the image plus
     * the zoomed width width and height. 
     * while zooming out the zoomed width and height is negative
     */
    public Dimension getPreferredSize()
    {
     if(m_image!=null){
        return new Dimension((int)(m_image.getWidth(this) + 
                                  (m_image.getWidth(this) * (m_zoom - 1))),
                             (int)(m_image.getHeight(this) + 
                                  (m_image.getHeight(this) * (m_zoom -1 ))));
     }else return null;
    }
    
    /**
     * Sets the new zoomed percentage
     * @param zoomPercentage
     */
    public void setZoomPercentage(int zoomPercentage)
    {
        m_zoomPercentage = ((double)zoomPercentage) / 100;    
    }
    
    /**
     * This method set the image to the original size
     * by setting the zoom factor to 1. i.e. 100%
     */
    public void originalSize()
    {
        m_zoom = 1; 
    }
    
    /**
     * This method increments the zoom factor with
     * the zoom percentage, to create the zoom in effect 
     */
    public void zoomIn()
    {
        m_zoom += m_zoomPercentage;
    }            
    
    /**
     * This method decrements the zoom factor with the 
     * zoom percentage, to create the zoom out effect 
     */
    public void zoomOut()
    {
        m_zoom -= m_zoomPercentage;
        
        if(m_zoom < m_zoomPercentage)
        {
            if(m_zoomPercentage > 1.0)
            {
                m_zoom = 1.0;
            }
            else
            {
                zoomIn();
            }
        }
    }
    
    /**
     * This method returns the currently
     * zoomed percentage
     * 
     * @return
     */
    public double getZoomedTo()
    {
        return m_zoom * 100; 
    }

      
    public void adjustLayout()
    {
        m_imageContainer.doLayout();        
        m_srollPane.doLayout();
        m_srollPane.validate();      
    }
    
  

	public ImageContainer getM_imageContainer() {
		return m_imageContainer;
	}

	public void setM_imageContainer(ImageContainer container) {
		m_imageContainer = container;
	}

	public UniversalScrollPane getM_srollPane() {
		return m_srollPane;
	}

	public void setM_srollPane(UniversalScrollPane pane) {
		m_srollPane = pane;
	}

	
	public void disposeElement(){
		m_zoom = 0;
	    m_zoomPercentage=0;
	    m_image=null;            
	    m_zoomCursor=null;
	    m_srollPane=null;
	    m_imageContainer=null;
	}

	
	
	
	public void actionPerformed(ActionEvent e) {
		
		if(e.getActionCommand().equals("editRGraph")){
			
			BiochamDynamicTree.workbench.replaceTabbedPane(BiochamDynamicTree.currentModel).setVisible(true);
		}else if(e.getActionCommand().equals("printDotImage")){
			
		}else if(e.getActionCommand().equals("saveDotImage")){
			
			SwingWorker sw=new SwingWorker(){

				//File f1;
				Object[] possibilities;
				String extension;
				
				@Override
				public Object construct() {
					String[] imageFormats=ImageIO.getWriterFormatNames();
					
					

					for(int i=0;i<imageFormats.length;i++){
						imageFormats[i]= imageFormats[i].toLowerCase();
					}
					java.util.Arrays.sort(imageFormats);
					int counter=0;
					for(int i=0,j=i+1;i<imageFormats.length-1;i++,j++){
						if(!imageFormats[i].equals(imageFormats[j])){
							counter++;
						}
					}				   
					possibilities = new String[counter];
					for(int i=0,j=i+1,k=0;i<imageFormats.length-1;i++,j++){
						if(!imageFormats[i].equals(imageFormats[j])){
							possibilities[k]=imageFormats[i];
							k++;
						}
					}
					return possibilities;
				}
				
				@Override
				public void finished(){
					String s = (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,
							" \nChoose Save Format:\n",
							"Save Image As",
							JOptionPane.PLAIN_MESSAGE,
							Icons.icons.get("File-1-48x48.png"+1.5),
							(Object[]) getValue(),
							"png");
					if(s!=null){
						extension=s;
					}else{
						extension="png";
					}
					final String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,"image");			
					if (rep!=null) {
						SwingWorker ss=new SwingWorker(){

							@Override
							public Object construct() {
								saveToImage(rep,extension);
								return null;
							}

							@Override
							public void finished() {
								// TODO Auto-generated method stub
								
							}};
						ss.start();
						 
			    	}
				}
				
			};
			
			sw.start();
			
		}else if(e.getActionCommand().equals("zoomIn")){
			
			  zoomIn();       
			  adjustLayout();
			  
		}else if(e.getActionCommand().equals("zoomOut")){
			
			  zoomOut();
			  adjustLayout();
			  
		}
		
	}
}
