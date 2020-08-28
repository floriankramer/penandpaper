#pragma once

#include <QPushButton>
#include <QSlider>
#include <QWidget>

namespace atlas {
class MapWidget;

class BrushPanel : public QWidget {
  Q_OBJECT

 public:
  BrushPanel(QWidget *parent = nullptr);
  virtual ~BrushPanel();

  void setMapWidget(MapWidget *map_widget);

 private:
  void createUi();

  MapWidget *_map_widget;

  QPushButton *_add_button;
  QPushButton *_subtract_button;
  QPushButton *_noise_button;
  QPushButton *_smoothe_button;
  QPushButton *_sharpen_button;

  QSlider *_brush_size_slider;
  QSlider *_brush_strength_slider;
};

}  // namespace atlas
