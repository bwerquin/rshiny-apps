/**
 * Rules API.
 * GET /rest/suivi
 * GET /rest/rules/:id
 */

const _ = require('lodash');
const suivis = require('./suivi');

function checkRule(rule) {
  const title = rule.title;
  const desc = rule.description;

  return !!title && title.length <= 50 &&
    (!desc || (desc.length >= 5 && desc.length <= 100));
}

module.exports = function rulesRouter(app) {
  let id = _.last(suivis).id;

  app.get('/rest/suivi', (req, res) => {
    console.info('GET /rest/suivi');
    return res.status(200).json(suivis);
  });

  app.get('/rest/suivi/:id', (req, res) => {
    console.info(`GET /rest/suivi/${req.params.id}`);

    const paramId = Number(req.params.id);
    const suivi = _.find(suivis, r => r.id === paramId);

    if (!suivi) {
      return res.status(404).send();
    }
    return res.status(200).json(suivi);
  });

};
