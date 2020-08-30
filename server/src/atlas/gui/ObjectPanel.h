#pragma once

#include <QWidget>
#include <memory>
#include <QListWidget>

#include "core/Map.h"
#include "core/ObjectIndex.h"

namespace atlas {

class MapWidget;

class ObjectPanel : public QWidget {
  Q_OBJECT

 public:
  ObjectPanel(QWidget *parent = nullptr);
  virtual ~ObjectPanel();

  void setMap(std::shared_ptr<Map> map);
  void setMapWidget(MapWidget *widget);

 private:
  void createUi();

  void updateDistributions();

  void createNewDistribution();

  std::shared_ptr<Map> _map;
  MapWidget *_map_widget;

  ObjectIndex _object_index;

  QListWidget *_distributions_list = new QListWidget();
};

}  // namespace atlas
