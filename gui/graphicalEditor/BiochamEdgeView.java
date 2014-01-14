package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.CellView;
import org.jgraph.graph.CellViewRenderer;
import org.jgraph.graph.EdgeRenderer;
import org.jgraph.graph.EdgeView;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.PortView;
import org.jgraph.util.Bezier;
import org.jgraph.util.ParallelEdgeRouter;
import org.jgraph.util.Spline2D;

import java.awt.Polygon;
import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;



public class BiochamEdgeView extends EdgeView{

	
	
	
	public transient CellViewRenderer renderer = new BiochamEdgeRenderer();
	//protected CellViewRenderer renderer = new BiochamEdgeRenderer();
	protected BiochamEdgeData userObject;
	Point2D firstPoint, secondPoint;
	
	public BiochamEdgeView(Object cell){
		super(cell);
		
		BiochamGraphConstants.setLineStyle(getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
		if(cell instanceof BiochamEdge){
			userObject=((BiochamEdgeData)((BiochamEdge)cell).getUserObject());	
		//	BiochamGraphConstants.setRouting(((BiochamEdge)cell).getAttributes(),BiochamGraphConstants.ROUTING_DEFAULT);
		//	BiochamGraphConstants.setLineStyle(((BiochamEdge)cell).getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
		//	BiochamGraphConstants.setBendable(((BiochamEdge)cell).getAttributes(), true);
			 /* BiochamGraphConstants.setLineStyle(((BiochamEdge)cell).getAttributes(), BiochamGraphConstants.STYLE_BEZIER);  
			  BiochamGraphConstants.setRouting(((BiochamEdge)cell).getAttributes(), ParallelEdgeRouter.getSharedInstance());*/
			  /*if(renderer!=null){
				  if(((BiochamEdgeRenderer)renderer).getFirstPoint()!=null){
					  firstPoint=((BiochamEdgeRenderer)renderer).getFirstPoint();
				  }
				  if(((BiochamEdgeRenderer)renderer).getLastPoint()!=null){
					  secondPoint=((BiochamEdgeRenderer)renderer).getLastPoint();
				  }				  
			  }
			  userObject.setFirstPoint(firstPoint);
			  userObject.setLastPoint(secondPoint);*/
			  /*CellView[] cls=this.getChildViews();
			  if(cls!=null){
				  for(int i=0;i<cls.length;i++){
					  if(cls[i] instanceof PortView){
						  ((PortView)cls[i]).allowPortMagic=true;
						  System.out.println("OKL");
					  }
				  }
			  }*/
			/*if(userObject.getStoichiometry()>0){
				BiochamGraphConstants.setLabelAlongEdge(this.getAttributes(), true);
				BiochamGraphConstants.setLabelEnabled(this.getAttributes(),true);
				BiochamGraphConstants.setExtraLabels(this.getAttributes(),new Object[]{new JLabel(Integer.toString(userObject.getStoichiometry()))});
				//Integer.toString(userObject.getStoichiometry());
			}*/
		}
		
		
	}

	/**
	080:             * Overrides the parent's implementation to return the moveable port
	081:             * position for this edge.
	082:             * 
	083:             * @param isSource
	084:             *            Whether to return the nearest point for the source or target.
	085:             * @return Returns the moveable port position for this edge.
	086:             */
	            protected Point2D getNearestPoint(boolean isSource) {
	                if ((isSource && getSource() != null)
	                        || (!isSource && getTarget() != null)) {
	                    Point2D offset = (isSource) ? offset = JGraphpadGraphConstants
	                            .getSourcePortOffset(getAllAttributes())
	                            : JGraphpadGraphConstants
	                                    .getTargetPortOffset(getAllAttributes());
	                    CellView parentView = (isSource) ? getSource()
	                            .getParentView() : getTarget().getParentView();
	
	                    // Computes the absolute location of the relative offset
	                    // and the source or target parentview's bounds.
	                    if (offset != null && parentView != null) {
	                        Rectangle2D r = parentView.getBounds();
	                        double x = offset.getX() * (r.getWidth() - 1)
	                                / GraphConstants.PERMILLE;
	                        double y = offset.getY() * (r.getHeight() - 1)
	                                / GraphConstants.PERMILLE;
	                        Point2D pos = new Point2D.Double(r.getX() + x, r.getY()
	                               + y);
	                        return pos;
	                    }
	               }
	                return super .getNearestPoint(isSource);
	            }
	
	           

	public class BiochamEdgeRenderer extends EdgeRenderer {
		
		//int beginDeco,endDeco,lineStyle,beginSize,endSize;
		//boolean beginFill,endFill;
		
			
		
		
		@Override
		public Shape createLineEnd(int size, int style, Point2D src, Point2D dst) {
			if (src == null || dst == null)
				return null;
			int d = (int) Math.max(1, dst.distance(src));
			int ax = (int) -(size * (dst.getX() - src.getX()) / d);
			int ay = (int) -(size * (dst.getY() - src.getY()) / d);
			if (style == BiochamGraphConstants.ARROW_DIAMOND) {
				Polygon poly = new Polygon();
				poly.addPoint((int) dst.getX(), (int) dst.getY());
				poly.addPoint((int) (dst.getX() + ax / 2 + ay / 3), (int) (dst
						.getY()
						+ ay / 2 - ax / 3));
				Point2D last = (Point2D) dst.clone();
				dst.setLocation(dst.getX() + ax, dst.getY() + ay);
				poly.addPoint((int) dst.getX(), (int) dst.getY());
				poly.addPoint((int) (last.getX() + ax / 2 - ay / 3), (int) (last
						.getY()
						+ ay / 2 + ax / 3));
				return poly;

			} else if (style == BiochamGraphConstants.ARROW_TECHNICAL || style == BiochamGraphConstants.ARROW_CLASSIC) {
				Polygon poly = new Polygon();
				poly.addPoint((int) dst.getX(), (int) dst.getY());
				poly.addPoint((int) (dst.getX() + ax + ay / 2), (int) (dst.getY()
						+ ay - ax / 2));
				Point2D last = (Point2D) dst.clone();
				if (style == BiochamGraphConstants.ARROW_CLASSIC) {
					dst.setLocation((int) (dst.getX() + ax * 2 / 3), (int) (dst
							.getY() + ay * 2 / 3));
					poly.addPoint((int) dst.getX(), (int) dst.getY());
				} else if (style == BiochamGraphConstants.ARROW_DIAMOND) {
					dst.setLocation(dst.getX() + 2 * ax, dst.getY() + 2 * ay);
					poly.addPoint((int) dst.getX(), (int) dst.getY());
				} else
					dst.setLocation((int) (dst.getX() + ax),
							(int) (dst.getY() + ay));
				poly.addPoint((int) (last.getX() + ax - ay / 2), (int) (last.getY()
						+ ay + ax / 2));
				return poly;

			} else if (style == BiochamGraphConstants.ARROW_SIMPLE) {
				GeneralPath path = new GeneralPath(GeneralPath.WIND_NON_ZERO, 4);
				path.moveTo((float) (dst.getX() + ax + ay / 2), (float) (dst.getY()
						+ ay - ax / 2));
				path.lineTo((float) dst.getX(), (float) dst.getY());
				path.lineTo((float) (dst.getX() + ax - ay / 2), (float) (dst.getY()	+ ay + ax / 2));
				return path;

			} else if (style == BiochamGraphConstants.ARROW_CIRCLE) {
				Ellipse2D ellipse = new Ellipse2D.Float((float) (dst.getX() + ax
						/ 2 - size/4), (float) (dst.getY() + ay / 2 - size/4 ),
						size/2, size/2);
				dst.setLocation(dst.getX() + ax, dst.getY() + ay);
				return ellipse;

			} else if (style == BiochamGraphConstants.ARROW_LINE || style == BiochamGraphConstants.ARROW_DOUBLELINE) {
				GeneralPath path = new GeneralPath(GeneralPath.WIND_NON_ZERO, 4);
				path.moveTo((float) (dst.getX() + ax / 2 + ay / 2), (float) (dst.getY()	+ ay / 2 - ax / 2));
				path.lineTo((float) (dst.getX() + ax / 2 - ay / 2), (float) (dst.getY()	+ ay / 2 + ax / 2));
				if (style == BiochamGraphConstants.ARROW_DOUBLELINE) {
					path.moveTo((float) (dst.getX() + ax / 3 + ay / 2),	(float) (dst.getY() + ay / 3 - ax / 2));
					path.lineTo((float) (dst.getX() + ax / 3 - ay / 2), (float) (dst.getY()	+ ay / 3 + ax / 2));
				}
				return path;
			} /*else if (style == BiochamGraphConstants.ARROW_ASSOC){
				
			
				Ellipse2D ellipse = new Ellipse2D.Float((float) (dst.getX() + ax
						/ 2 - size/4), (float) (dst.getY() + ay / 2 - size/4 ),
						size/2, size/2);
				dst.setLocation(dst.getX() + ax, dst.getY() + ay);
				//path.append(ellipse, true);
								
				return ellipse;
				
			} else if (style == BiochamGraphConstants.ARROW_DISSOC){
				
				
				
			} else if (style == BiochamGraphConstants.ARROW_INHIBITION){
				GeneralPath path = new GeneralPath();
				path.moveTo((float) (dst.getX() + ax / 2 + ay / 2), (float) (dst.getY()	+ ay / 2 - ax / 2));
				path.lineTo((float) (dst.getX() + ax / 2 - ay / 2), (float) (dst.getY()	+ ay / 2 + ax / 2));
				return path;
				
			}*/
			
			return null;
		}
		
		@Override
		public Shape createShape() {
			

			int d=0,ax = 0,ay=0;
			
		
			try{
				firstPoint=view.getPoint(0);
				secondPoint=view.getPoint(view.getPointCount()-1);
			}catch(NullPointerException e){}
			//System.out.println("first: x="+firstPoint.getX()+",y="+firstPoint.getY());
			//System.out.println("second: x="+secondPoint.getX()+",y="+secondPoint.getY());
		
					
		//	view.addPoint(view.getPointCount(), new Point2D.Double(BiochamGraphConstants.PERMILLE/3,view.getPoint(0).getY()-5));
		//	view.addPoint(view.getPointCount(), new Point2D.Double(2*BiochamGraphConstants.PERMILLE/3,view.getPoint(view.getPointCount()-1).getY()+5));
			int n = view.getPointCount();
			
			if(lineStyle==BiochamGraphConstants.STYLE_BEZIER && n<=2){
				//view.addPoint(view.getPointCount(), new Point2D.Double(BiochamGraphConstants.PERMILLE*7/8,+2));
				//view.addPoint(view.getPointCount(), new Point2D.Double(BiochamGraphConstants.PERMILLE/8,+2));
				//view.addPoint(view.getPointCount(),new Point2D.Double(firstPoint.getX()+1,firstPoint.getY()+10));
			//	view.addPoint(view.getPointCount(),new Point2D.Double(firstPoint.getX()+4,firstPoint.getY()+20));	
				//n=view.getPointCount();
			}
			/*System.out.println("\n\n n is="+n);
			for(int r=0;r<n;r++){
				System.out.println("index="+r+",("+view.getPoint(r).getX()+","+view.getPoint(r).getX()+").");
			}*/
			//System.out.println("\n\n\n");
			/*if(n<4){
				//n+=(6-n);
				for(int k=0;k<4-n;k++){
					view.addPoint(view.getPointCount(),new Point2D.Double(view.getPoint(n-1).getX(),view.getPoint(n-1).getX()));
					System.out.println("Adding.........index="+n+",("+view.getPoint(n-1).getX()+","+view.getPoint(n-1).getX()+").");
				}
				n=view.getPointCount();
			}
			System.out.println("\n\n AFTER n is="+n);*/
			if (n > 1) {
				// Following block may modify static vars as side effect (Flyweight
				// Design)
				EdgeView tmp = view;
				Point2D[] p = null;
				p = new Point2D[n];
				for (int i = 0; i < n; i++) {
					if(tmp!=null){
						try{
							Point2D pt = tmp.getPoint(i);
							if (pt == null)
								return null; // exit
							p[i] = new Point2D.Double(pt.getX(), pt.getY());
						}catch(Exception e){
							e.printStackTrace();
							break;
						}
						
					}
				}

				// End of Side-Effect Block
				// Undo Possible MT-Side Effects
				if (view != tmp) {
					view = tmp;
					installAttributes(view);
				}
				// End of Undo
				if (view.sharedPath == null) {
					view.sharedPath = new GeneralPath(GeneralPath.WIND_NON_ZERO, n);
				} else {
					view.sharedPath.reset();
				}
				view.beginShape = view.lineShape = view.endShape = null;
				Point2D p0 = p[0];
				Point2D pe = p[n - 1];				
				Point2D p1 = p[1];
				Point2D p2 = p[n - 2];
			
				
				if (lineStyle == BiochamGraphConstants.STYLE_BEZIER && n > 2) {
					bezier = new Bezier(p);
					p2 = bezier.getPoint(bezier.getPointCount() - 1);
				} else if (lineStyle == GraphConstants.STYLE_SPLINE && n > 2) {
					spline = new Spline2D(p);
					double[] point = spline.getPoint(0.9875);
					// Extrapolate p2 away from the end point, pe, to avoid integer
					// rounding errors becoming too large when creating the line end
					double scaledX = pe.getX() - ((pe.getX() - point[0]) * 128);
					double scaledY = pe.getY() - ((pe.getY() - point[1]) * 128);
					p2.setLocation(scaledX, scaledY);
				}

				if (beginDeco != BiochamGraphConstants.ARROW_NONE) {
					view.beginShape = createLineEnd(beginSize, beginDeco, p1, p0);
				}
				if (endDeco != BiochamGraphConstants.ARROW_NONE) {
					if(endDeco==12){
			
						d = (int) Math.max(1, pe.distance(p2));
						ax = (int) -(endSize * (pe.getX() - p2.getX()) / d);
						ay = (int) -(endSize * (pe.getY() - p2.getY()) / d);
						view.endShape = createLineEnd(endSize, endDeco, p2, pe);
						
					}else{
						view.endShape = createLineEnd(endSize, endDeco, p2, pe);
					}
				}
				view.sharedPath.moveTo((float) p0.getX(), (float) p0.getY());
		
				/* THIS CODE WAS ADDED BY MARTIN KRUEGER 10/20/2003 */
				if (lineStyle == BiochamGraphConstants.STYLE_BEZIER && n > 2) {
					Point2D[] b = bezier.getPoints();
					view.sharedPath.quadTo((float) b[0].getX(),
							(float) b[0].getY(), (float) p1.getX(), (float) p1
									.getY());
					for (int i = 2; i < n - 1; i++) {
						Point2D b0 = b[2 * i - 3];
						Point2D b1 = b[2 * i - 2];
						view.sharedPath.curveTo((float) b0.getX(), (float) b0
								.getY(), (float) b1.getX(), (float) b1.getY(),
								(float) p[i].getX(), (float) p[i].getY());
					}
					view.sharedPath.quadTo((float) b[b.length - 1].getX(),
							(float) b[b.length - 1].getY(),
							(float) p[n - 1].getX(), (float) p[n - 1].getY());
				} else if (lineStyle == BiochamGraphConstants.STYLE_SPLINE && n > 2) {
					for (double t = 0; t <= 1; t += 0.0125) {
						double[] xy = spline.getPoint(t);
						view.sharedPath.lineTo((float) xy[0], (float) xy[1]);
					}
				}
				/* END if its supposed to be a normal line*/
				else {
					if(endDeco==12){
						d = (int) Math.max(1, pe.distance(p2));
						int d2= (int)Math.sqrt(Math.pow(pe.getX()-p2.getX(),2)+Math.pow(pe.getY()-p2.getY(),2));
						ax = (int) -(2 * (pe.getX() - p2.getX()) / d);
						ay = (int) -(2 * (pe.getY() - p2.getY()) / d);
						p1.setLocation(p1.getX()+3*ax, p1.getY()+3*ay);
						for (int i = 1; i < n - 1; i++)							
							view.sharedPath.lineTo((float) p[i].getX(), (float) p[i].getY());
					
							view.sharedPath.lineTo((float) pe.getX(), (float) pe.getY());
	
					}else{
						for (int i = 1; i < n - 1; i++)
							view.sharedPath.lineTo((float) p[i].getX(), (float) p[i].getY());
					
						view.sharedPath.lineTo((float) pe.getX(), (float) pe.getY());
						Point2D current=view.sharedPath.getCurrentPoint();					
						//double xm0=(pe.getX()-current.getX())/2;
						//double ym0=(pe.getY()-current.getY())/2;
					
						
					}
					view.sharedPath.moveTo((float) pe.getX(), (float) pe.getY());
				}

				if (view.endShape == null && view.beginShape == null) {
					// With no end decorations the line shape is the same as the
					// shared path and memory
					view.lineShape = view.sharedPath;
				} else {
					view.lineShape = (GeneralPath) view.sharedPath.clone();
					if (view.endShape != null)
						if(view.sharedPath!=null){
							view.sharedPath.append(view.endShape, true);		
						}											
					if (view.beginShape != null)
						view.sharedPath.append(view.beginShape, true);
				}
			
				return view.sharedPath;
				
			}
			return null;
		}
		
		public EdgeView getView(){
			return view;
		}
		public Point2D getFirstPoint(){
			if(view!=null){
				return view.getPoint(0);
			}else{
				return null;
			}
			
		}
		public Point2D getLastPoint(){
			if(view!=null){
				return view.getPoint(view.getPointCount()-1);
			}else{
				return null;
			}
			
		}
	}

	 /** Returns the renderer for the view. */
    public CellViewRenderer getRenderer() {
      return renderer;
    }
  
    /**
     * Change the renderer of this view.
     *
     * @param renderer a <code>CellViewRenderer</code>. if <code>renderer</code> is null, 
     * this method create a default renderer.
     */
    
    public void setRenderer(CellViewRenderer renderer){
	this.renderer = renderer;
	if (this.renderer == null) {
	    this.renderer = new BiochamEdgeRenderer();
	}
    }

   

}
