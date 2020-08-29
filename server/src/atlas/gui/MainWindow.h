#pragma once

#include <QMainWindow>
#include <memory>

#include "BrushPanel.h"
#include "MapWidget.h"
#include "ObjectPanel.h"
#include "core/Map.h"
#include "core/ObjectIndex.h"

namespace atlas {
class MainWindow : public QMainWindow {
  Q_OBJECT
 public:
  MainWindow();

 private:
  void buildUi();
  void buildMenu();

  void newMap();

  MapWidget *_map_widget;
  BrushPanel *_brush_panel;
  ObjectPanel *_object_panel;

  std::shared_ptr<Map> _map;
};
}  // namespace atlas
