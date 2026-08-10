/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

sirius.ready(() => {
    const _container = document.getElementById('timing-container');
    const _cardTemplate = document.getElementById('category-card-template');
    const _searchInput = document.getElementById('timing-filter');
    const _searchButton = document.getElementById('timing-search');
    const _reloadButton = document.getElementById('timing-reload');
    const _toggleButton = document.getElementById('timing-toggle');
    const _csrfToken = document.getElementById('timing-csrf-token');
    const _toggleIcon = document.getElementById('timing-toggle-icon');
    const _toggleLabel = document.getElementById('timing-toggle-label');
    const _statusActive = document.getElementById('timing-status-active');
    const _statusInactive = document.getElementById('timing-status-inactive');

    let rows = [];

    const sorting = {
        // A single sort state shared by all tables so that sorting one table sorts all of them the same way.
        state: {key: 'key', ascending: true},

        // Toggles the order if the same key is clicked again, otherwise starts
        // sorting by the newly selected key.
        toggle: function (key) {
            if (this.state.key === key) {
                this.state.ascending = !this.state.ascending;
            } else {
                this.state = {key: key, ascending: false};
            }
        },

        // Returns a sorted copy of the given rows according to the current sort state.
        apply: function (entries) {
            return [...entries].sort((a, b) => {
                const valueA = a[this.state.key];
                const valueB = b[this.state.key];
                const comparison = typeof valueA === 'string' ? valueA.localeCompare(valueB) : valueA - valueB;
                return this.state.ascending ? comparison : -comparison;
            });
        },

        // Determines which sort icon a column header should show for the current state.
        icon: function (key) {
            if (this.state.key !== key) {
                return 'fa-sort text-muted';
            }
            return this.state.ascending ? 'fa-sort-up' : 'fa-sort-down';
        }
    };

    // Keeps only the rows matching the current search term (by key or category).
    function filterRows(list, search) {
        return list.filter((row) => {
            return row.key.toLowerCase().includes(search)
                || row.category.toLowerCase().includes(search);
        });
    }

    // Groups the given rows by their category into a Map of category -> [rows].
    function groupByCategory(list) {
        const groups = new Map();
        list.forEach((row) => {
            if (!groups.has(row.category)) {
                groups.set(row.category, []);
            }
            groups.get(row.category).push(row);
        });
        return groups;
    }

    // Builds a single table row for the given data row.
    function buildRow(row) {
        const _row = document.createElement('tr');

        const _tdKey = document.createElement('td');
        _tdKey.className = 'text-break align-top';
        _tdKey.textContent = row.key;
        _row.appendChild(_tdKey);

        const _tdCount = document.createElement('td');
        _tdCount.className = 'text-end text-nowrap align-top';
        _tdCount.textContent = row.count;
        _row.appendChild(_tdCount);

        const _tdAverageMillis = document.createElement('td');
        _tdAverageMillis.className = 'text-end text-nowrap align-top';
        _tdAverageMillis.textContent = row.averageMillis.toFixed(2) + " (ms)";
        _row.appendChild(_tdAverageMillis);

        return _row;
    }

    // sessionStorage key under which the current view (sort state and filter) is kept so it survives a full reload.
    const VIEW_STORAGE_KEY = 'system-timing-view';

    // Stores the current sort state and filter so they survive a full page reload (e.g. F5 or Cmd+R)
    function persistState() {
        sessionStorage.setItem(VIEW_STORAGE_KEY, JSON.stringify({
            sorting: sorting.state,
            filter: _searchInput.value
        }));
    }

    // Restores the sort state and filter persisted before a reload, if any were stored.
    function restoreState() {
        const stored = sessionStorage.getItem(VIEW_STORAGE_KEY);
        if (!stored) {
            return;
        }
        const view = JSON.parse(stored);
        sorting.state = view.sorting || {key: 'key', ascending: true};
        _searchInput.value = view.filter || '';
    }

    // Renders one card (with a sortable table) per category into the container.
    function render() {
        const search = _searchInput.value.toLowerCase();
        const groups = groupByCategory(filterRows(rows, search));

        _container.innerHTML = '';
        groups.forEach((entries, category) => {
            const sorted = sorting.apply(entries);

            const _card = _cardTemplate.content.cloneNode(true);
            _card.querySelector('.card-title').textContent = category;
            _card.querySelectorAll('th[data-sort]').forEach((_header) => {
                _header.querySelector('i').className = 'fa-solid ' + sorting.icon(_header.dataset.sort);
            });

            const _tbody = _card.querySelector('tbody');
            sorted.forEach((row) => {
                _tbody.appendChild(buildRow(row));
            });

            _container.appendChild(_card);
        });

        persistState();
    }

    // Fetches the timing data from the server and renders it.
    function loadData() {
        sirius.getJSON('/system/timing/api', {})
            .then((data) => {
                rows = data.timings || [];
                render();
            })
            .catch((error) => {
                console.error('Could not load timing data:', error);
            });
    }

    // Reload only fetches while collecting; when disabled it would wipe the frozen data, so it does nothing.
    function reload() {
        if (_toggleButton.dataset.enabled === 'true') {
            loadData();
        }
    }

    // Enables/disables the micro timing without reloading the page.
    function toggleTiming() {
        const enable = _toggleButton.dataset.enabled !== 'true';
        sirius.postJSON('/system/timing/' + (enable ? 'enable' : 'disable'), {CSRFToken: _csrfToken.value})
            .then((data) => {
                updateToggleUI(data.enabled);
                if (data.enabled) {
                    loadData();
                }
            })
            .catch((error) => {
                console.error('Could not toggle timing:', error);
            });
    }

    // Reflects the new enabled state on the button and the status dot (without touching the table).
    function updateToggleUI(enabled) {
        _toggleButton.dataset.enabled = enabled;
        _toggleIcon.innerHTML = '<i class="fa-solid ' + (enabled ? 'fa-minus' : 'fa-plus') + '"></i>';
        _toggleLabel.textContent = enabled ? 'Disable' : 'Enable';
        _statusActive.classList.toggle('d-none', !enabled);
        _statusInactive.classList.toggle('d-none', enabled);
        _reloadButton.classList.toggle('disabled', !enabled);
    }

    // Sorting: clicking or pressing Enter/Space on a sortable column header toggles the sort order for all tables.
    function toggleSortFor(_header) {
        sorting.toggle(_header.dataset.sort);
        render();
    }

    _container.addEventListener('click', (event) => {
        const _header = event.target.closest('th[data-sort]');
        if (!_header) {
            return;
        }
        toggleSortFor(_header);
    });

    _container.addEventListener('keydown', (event) => {
        if (event.key !== 'Enter' && event.key !== ' ') {
            return;
        }
        const _header = event.target.closest('th[data-sort]');
        if (!_header) {
            return;
        }
        event.preventDefault();
        toggleSortFor(_header);
    });

    sirius.addEnterListener(_searchInput, render);
    sirius.addClickOrEnterListener(_searchButton, render);

    sirius.addClickOrEnterListener(_reloadButton, reload);
    _reloadButton.classList.toggle('disabled', _toggleButton.dataset.enabled !== 'true');
    sirius.addClickOrEnterListener(_toggleButton, toggleTiming);

    restoreState();
    loadData();
});
