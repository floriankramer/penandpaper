#include "ObjectPanel.h"

#include <QHBoxLayout>
#include <QPushButton>
#include <QVBoxLayout>

#include "MapWidget.h"

namespace atlas {

ObjectPanel::ObjectPanel(QWidget *parent) : QWidget(parent), _map(nullptr) {
  _object_index.load("objects");
  createUi();
}

ObjectPanel::~ObjectPanel() {}

void ObjectPanel::setMap(std::shared_ptr<Map> map) { _map = map; }

void ObjectPanel::setMapWidget(MapWidget *widget) { _map_widget = widget; }

void ObjectPanel::createUi() {
  QVBoxLayout *layout = new QVBoxLayout();

  QHBoxLayout *button_layout = new QHBoxLayout();

  QPushButton *new_distribution = new QPushButton("+");
  connect(new_distribution, &QPushButton::pressed, [this] {
    if (_map != nullptr) {
      ObjectDistribution dist(_map->widthPixels(), _map->heightPixels(),
                              _object_index.objects()[0].image);
      _map->addObjectDistribution(dist);
      // TODO: this is actually not required and only used for testing
      _map_widget->invalidateObjectLayer();
    }
  });

  button_layout->addWidget(new_distribution);

  layout->addLayout(button_layout);

  layout->addWidget(new QWidget(), 1);
  setLayout(layout);
}

}  // namespace atlas
